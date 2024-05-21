use std::sync::{Arc, Mutex};

use salsa::DebugWithDb;

use crate::{
    item::ParamData, Expr, ExprId, Function, HirFile, HirMasterDatabase, Item, Literal, LowerError,
    Module, ModuleKind, Path, Pod, Pods, ResolutionMap, ResolutionStatus, Stmt, Struct, StructKind,
    Symbol, Type, UnaryOp, UseItem,
};

/// SalsaのDB
#[derive(Default)]
#[salsa::db(crate::Jar)]
pub struct TestingDatabase {
    storage: salsa::Storage<Self>,

    /// テスト用のログ
    logs: Option<Arc<Mutex<Vec<String>>>>,
}

impl TestingDatabase {
    #[cfg(test)]
    #[allow(dead_code)]
    pub fn enable_logging(self) -> Self {
        assert!(self.logs.is_none());
        Self {
            storage: self.storage,
            logs: Some(Default::default()),
        }
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub fn take_logs(&mut self) -> Vec<String> {
        if let Some(logs) = &self.logs {
            std::mem::take(&mut *logs.lock().unwrap())
        } else {
            panic!("logs not enabled");
        }
    }
}

impl salsa::Database for TestingDatabase {
    fn salsa_event(&self, event: salsa::Event) {
        if let Some(logs) = &self.logs {
            logs.lock()
                .unwrap()
                .push(format!("Event: {:?}", event.debug(self)));
        }
    }
}

impl salsa::ParallelDatabase for TestingDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(TestingDatabase {
            storage: self.storage.snapshot(),
            logs: self.logs.clone(),
        })
    }
}

pub struct Pretty<'a> {
    db: &'a dyn HirMasterDatabase,
    #[allow(dead_code)]
    pods: &'a Pods,
}
impl<'a> Pretty<'a> {
    pub fn new(db: &'a dyn HirMasterDatabase, pods: &'a Pods) -> Self {
        Self { db, pods }
    }

    pub fn format_mutable(mutable: bool) -> &'static str {
        if mutable {
            "mut "
        } else {
            ""
        }
    }

    pub fn format_indent(nesting: usize) -> String {
        "    ".repeat(nesting)
    }

    pub fn format_pods(&self, pods: &Pods) -> String {
        self.format_pod(&pods.root_pod, &pods.resolution_map)
    }

    fn format_pod(&self, pod: &Pod, resolution_map: &ResolutionMap) -> String {
        let mut msg = "".to_string();

        msg.push_str("//- /main.nail\n");
        msg.push_str(&PrettyFile::new(self.db, &pod.root_hir_file, resolution_map).format_file());

        for (file, hir_file) in pod.get_hir_files_order_registration_asc() {
            let file_path = file.file_path(self.db);
            msg.push_str(&format!("//- {}\n", file_path.to_string_lossy()));
            msg.push_str(&PrettyFile::new(self.db, hir_file, resolution_map).format_file());
        }

        msg
    }
}

struct PrettyFile<'a> {
    db: &'a dyn HirMasterDatabase,
    hir_file: &'a HirFile,
    resolution_map: &'a ResolutionMap,
}
impl<'a> PrettyFile<'a> {
    fn new(
        db: &'a dyn HirMasterDatabase,
        hir_file: &'a HirFile,
        resolution_map: &'a ResolutionMap,
    ) -> Self {
        Self {
            db,
            hir_file,
            resolution_map,
        }
    }

    fn format_file(&self) -> String {
        let mut msg = "".to_string();

        for item in self.hir_file.top_level_items(self.db) {
            msg.push_str(&self.format_item(item, 0));
        }

        for error in self.hir_file.errors(self.db) {
            match error {
                LowerError::UndefinedEntryPoint => {
                    msg.push_str("error: Undefined entry point.(help: fn main() { ... })\n");
                }
            }
        }

        msg
    }

    fn format_function(&self, function: Function, nesting: usize) -> String {
        let body_expr = function.body(self.db, *self.hir_file).unwrap();

        let name = function.name(self.db).text(self.db);
        let params = function
            .params(self.db)
            .iter()
            .map(|param| {
                let param_data = param.data(self.hir_file.db(self.db));
                self.format_parameter(param_data)
            })
            .collect::<Vec<_>>()
            .join(", ");
        let return_type = self.format_type(&function.return_type(self.db), nesting);

        let Expr::Block(block) = body_expr else {
            panic!("Should be Block")
        };

        let mut body = "{\n".to_string();
        for stmt in &block.stmts {
            body.push_str(&self.format_stmt(stmt, nesting + 1));
        }
        if let Some(tail) = block.tail {
            body.push_str(&format!(
                "{}expr:{}\n",
                Pretty::format_indent(nesting + 1),
                self.format_expr(tail, nesting + 1)
            ));
        }
        body.push_str(&format!("{}}}", Pretty::format_indent(nesting)));

        let is_entry_point = self.hir_file.entry_point(self.db) == Some(function);
        format!(
            "{}fn {}{name}({params}) -> {return_type} {body}\n",
            Pretty::format_indent(nesting),
            if is_entry_point { "entry:" } else { "" }
        )
    }

    fn format_struct(&self, structure: Struct, nesting: usize) -> String {
        let name = structure.name(self.db).text(self.db);

        let kind = match structure.kind(self.db) {
            StructKind::Tuple(tuple_fields) => {
                let fields = tuple_fields
                    .iter()
                    .map(|field| self.format_type(field, nesting))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({fields});")
            }
            StructKind::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|field| {
                        let name = field.name.text(self.db);
                        let ty = self.format_type(&field.ty, nesting);
                        format!("{name}: {ty}")
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{ {fields} }}")
            }
            StructKind::Unit => ";".to_string(),
        };

        format!(
            "{indent}struct {name} {kind}\n",
            indent = Pretty::format_indent(nesting)
        )
    }

    fn format_module(&self, module: Module, nesting: usize) -> String {
        let curr_indent = Pretty::format_indent(nesting);

        let module_name = module.name(self.db).text(self.db);

        match module.kind(self.db) {
            ModuleKind::Inline { items } => {
                let mut module_str = "".to_string();
                module_str.push_str(&format!("{curr_indent}mod {module_name} {{\n"));
                for (i, item) in items.iter().enumerate() {
                    module_str.push_str(&self.format_item(item, nesting + 1));
                    if i == items.len() - 1 {
                        continue;
                    }

                    module_str.push('\n');
                }
                module_str.push_str(&format!("{curr_indent}}}\n"));
                module_str
            }
            ModuleKind::Outline => {
                format!("{curr_indent}mod {module_name};\n")
            }
        }
    }

    fn format_use_item(&self, use_item: UseItem) -> String {
        let path_name = self.format_path(use_item.path(self.db));
        let item_name = use_item.name(self.db).text(self.db);

        format!("{path_name}::{item_name}")
    }

    fn format_item(&self, item: &Item, nesting: usize) -> String {
        match item {
            Item::Function(function) => self.format_function(*function, nesting),
            Item::Struct(struct_) => self.format_struct(*struct_, nesting),
            Item::Module(module) => self.format_module(*module, nesting),
            Item::UseItem(use_item) => self.format_use_item(*use_item),
        }
    }

    fn format_stmt(&self, stmt: &Stmt, nesting: usize) -> String {
        match stmt {
            Stmt::Let {
                name,
                binding,
                value,
            } => {
                let name = name.text(self.db);
                let expr_str = self.format_expr(*value, nesting);
                let binding = binding.lookup(self.hir_file.db(self.db));
                let mutable_text = Pretty::format_mutable(binding.mutable);
                format!(
                    "{}let {mutable_text}{name} = {expr_str}\n",
                    Pretty::format_indent(nesting)
                )
            }
            Stmt::Expr {
                expr,
                has_semicolon,
            } => format!(
                "{}{}{}\n",
                Pretty::format_indent(nesting),
                self.format_expr(*expr, nesting),
                if *has_semicolon { ";" } else { "" }
            ),
            Stmt::Item { item } => self.format_item(item, nesting),
        }
    }

    fn format_expr(&self, expr_id: ExprId, nesting: usize) -> String {
        match expr_id.lookup(self.hir_file.db(self.db)) {
            Expr::Symbol(symbol) => self.format_symbol(symbol, nesting),
            Expr::Literal(literal) => match literal {
                Literal::Bool(b) => b.to_string(),
                Literal::Char(c) => format!("'{c}'"),
                Literal::String(s) => format!("\"{s}\""),
                Literal::Integer(i) => i.to_string(),
            },
            Expr::Binary { op, lhs, rhs } => {
                let lhs_str = self.format_expr(*lhs, nesting);
                let rhs_str = self.format_expr(*rhs, nesting);
                format!("{lhs_str} {op} {rhs_str}")
            }
            Expr::Unary { op, expr } => {
                let op = match op {
                    UnaryOp::Neg => "-",
                    UnaryOp::Not => "!",
                };
                let expr_str = self.format_expr(*expr, nesting);
                format!("{op}{expr_str}")
            }
            Expr::Call { callee, args } => {
                let callee = self.format_symbol(callee, nesting);
                let args = args
                    .iter()
                    .map(|arg| self.format_expr(*arg, nesting))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{callee}({args})")
            }
            Expr::Block(block) => {
                let mut msg = "{\n".to_string();
                for stmt in &block.stmts {
                    msg.push_str(&self.format_stmt(stmt, nesting + 1));
                }
                if let Some(tail) = block.tail {
                    msg.push_str(&format!(
                        "{}expr:{}\n",
                        Pretty::format_indent(nesting + 1),
                        self.format_expr(tail, nesting + 1)
                    ));
                }
                msg.push_str(&format!("{}}}", Pretty::format_indent(nesting)));

                msg
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut msg = "if ".to_string();
                msg.push_str(&self.format_expr(*condition, nesting));
                msg.push(' ');
                msg.push_str(&self.format_expr(*then_branch, nesting));

                if let Some(else_branch) = else_branch {
                    msg.push_str(" else ");
                    msg.push_str(&self.format_expr(*else_branch, nesting));
                }

                msg
            }
            Expr::Return { value } => {
                let mut msg = "return".to_string();
                if let Some(value) = value {
                    msg.push_str(&format!(" {}", &self.format_expr(*value, nesting,)));
                }

                msg
            }
            Expr::Loop { block } => {
                let mut msg = "loop".to_string();
                msg.push_str(&format!(" {}", &self.format_expr(*block, nesting)));

                msg
            }
            Expr::Continue => "continue".to_string(),
            Expr::Break { value } => {
                let mut msg = "break".to_string();
                if let Some(value) = value {
                    msg.push_str(&format!(" {}", &self.format_expr(*value, nesting,)));
                }

                msg
            }
            Expr::Record { symbol, fields } => {
                let symbol = self.format_symbol(symbol, nesting);
                let fields = fields
                    .iter()
                    .map(|record_field| {
                        let value = self.format_expr(record_field.value, nesting);
                        format!("{name}: {value}", name = record_field.name.text(self.db))
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{symbol} {{ {fields} }}")
            }
            Expr::Field { base, name } => {
                let base = self.format_expr(*base, nesting);
                let name = name.text(self.db);

                format!("{base}.{name}")
            }
            Expr::Missing => "<missing>".to_string(),
        }
    }

    fn format_symbol(&self, symbol: &Symbol, nesting: usize) -> String {
        match &symbol {
            Symbol::Local { name, binding } => {
                let expr = binding.lookup(self.hir_file.db(self.db)).expr;
                match expr.lookup(self.hir_file.db(self.db)) {
                    Expr::Symbol { .. }
                    | Expr::Binary { .. }
                    | Expr::Missing
                    | Expr::Literal(_)
                    | Expr::Unary { .. }
                    | Expr::Call { .. }
                    | Expr::If { .. }
                    | Expr::Return { .. }
                    | Expr::Loop { .. }
                    | Expr::Continue
                    | Expr::Record { .. }
                    | Expr::Break { .. }
                    | Expr::Field { .. } => {
                        let expr_text = self.format_expr(expr, nesting);
                        format!("${}:{}", name.text(self.db), expr_text)
                    }
                    Expr::Block { .. } => name.text(self.db).to_string(),
                }
            }
            Symbol::Param { name, .. } => {
                let name = name.text(self.db);
                format!("param:{name}")
            }
            Symbol::MissingExpr { path } => {
                let resolving_status = self.resolution_map.item_by_symbol(path).unwrap();
                self.format_resolution_status(resolving_status)
            }
            Symbol::MissingType { path } => {
                let resolving_status = self.resolution_map.item_by_symbol(path).unwrap();
                self.format_resolution_status(resolving_status)
            }
        }
    }

    fn format_resolution_status(&self, resolution_status: ResolutionStatus) -> String {
        match resolution_status {
            ResolutionStatus::Unresolved => "<unknown>".to_string(),
            ResolutionStatus::Error => "<missing>".to_string(),
            ResolutionStatus::Resolved { path, item } => {
                let path = self.format_path(&path);
                match item {
                    Item::Function(_) => {
                        format!("fn:{path}")
                    }
                    Item::Struct(_) => {
                        format!("struct:{path}")
                    }
                    Item::Module(_) => {
                        format!("mod:{path}")
                    }
                    Item::UseItem(_) => {
                        unreachable!()
                    }
                }
            }
        }
    }

    fn format_path(&self, path: &Path) -> String {
        path.segments(self.db)
            .iter()
            .map(|segment| segment.text(self.db).to_string())
            .collect::<Vec<_>>()
            .join("::")
    }

    fn format_type(&self, ty: &Type, nesting: usize) -> String {
        match ty {
            Type::Integer => "int",
            Type::String => "string",
            Type::Char => "char",
            Type::Boolean => "bool",
            Type::Unit => "()",
            Type::Unknown => "<unknown>",
            Type::Custom(symbol) => {
                return self.format_symbol(symbol, nesting);
            }
        }
        .to_string()
    }

    fn format_parameter(&self, param_data: &ParamData) -> String {
        let name = if let Some(name) = param_data.name {
            name.text(self.db)
        } else {
            "<missing>"
        };
        let mutable_text = Pretty::format_mutable(param_data.mutable);
        let ty = self.format_type(&param_data.ty, 0);
        format!("{name}: {mutable_text}{ty}")
    }
}
