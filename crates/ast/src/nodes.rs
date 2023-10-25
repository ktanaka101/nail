use syntax::{SyntaxKind, SyntaxNode, SyntaxToken};

use crate::{
    ast_node::{self, Ast, AstNode, AstToken},
    tokens,
};

/// ASTノードを定義します。
///
/// # Example
///
/// このマクロは、`def_ast_node(VariableDef);`と呼び出すと以下のように展開されます。
/// ```ignore
/// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// pub struct VariableDef {
///     syntax: SyntaxNode,
/// }
///
/// impl Ast for VariableDef {}
/// impl AstNode for VariableDef {
///     fn can_cast(kind: SyntaxKind) -> bool {
///         kind == SyntaxKind::VariableDef
///     }
///
///     fn cast(syntax: SyntaxNode) -> Option<Self> {
///         if Self::can_cast(syntax.kind()) {
///             Some(Self { syntax })
///         } else {
///             None
///         }
///     }
///
///     fn syntax(&self) -> &SyntaxNode {
///         &self.syntax
///     }
/// }
/// ```
macro_rules! def_ast_node {
    ($(#[$meta:meta])* $kind:ident) => {
        /// ASTノード
        $(#[$meta])*
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $kind {
            syntax: SyntaxNode,
        }

        impl Ast for $kind {}
        impl AstNode for $kind {
            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::$kind
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self { syntax })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.syntax
            }
        }
    };
}

def_ast_node!(
    /// 変数定義のASTノード
    VariableDef
);
impl VariableDef {
    pub fn mut_token(&self) -> Option<SyntaxToken> {
        ast_node::token(&self.syntax, SyntaxKind::MutKw)
    }

    /// 変数の名前に位置するASTトークンを返します。
    pub fn name(&self) -> Option<tokens::Ident> {
        ast_node::child_token(self)
    }

    /// 変数に代入する式に位置する式ノードを返します。
    pub fn value(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }
}

def_ast_node!(
    /// 式文のASTノード
    ExprStmt
);
impl ExprStmt {
    /// 式に位置する式ノードを返します。
    pub fn expr(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }

    /// セミコロンに位置するASTトークンを返します。
    pub fn semicolon(&self) -> Option<tokens::Semicolon> {
        ast_node::child_token(self)
    }
}

/// 式ノード
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    /// 二項演算式
    BinaryExpr(BinaryExpr),
    /// リテラル
    Literal(Literal),
    /// 括弧式
    ParenExpr(ParenExpr),
    /// 単項演算式
    UnaryExpr(UnaryExpr),
    /// パス式
    PathExpr(PathExpr),
    /// 関数呼び出し
    CallExpr(CallExpr),
    /// ブロック式
    BlockExpr(BlockExpr),
    /// `if`式
    IfExpr(IfExpr),
    /// `return`式
    ReturnExpr(ReturnExpr),
    /// `loop`式
    LoopExpr(LoopExpr),
    /// `continue`式
    ContinueExpr(ContinueExpr),
    /// `break`式
    BreakExpr(BreakExpr),
    /// `while`式
    WhileExpr(WhileExpr),
}
impl Ast for Expr {}
impl AstNode for Expr {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::BinaryExpr
                | SyntaxKind::Literal
                | SyntaxKind::ParenExpr
                | SyntaxKind::UnaryExpr
                | SyntaxKind::PathExpr
                | SyntaxKind::CallExpr
                | SyntaxKind::BlockExpr
                | SyntaxKind::IfExpr
                | SyntaxKind::ReturnExpr
                | SyntaxKind::LoopExpr
                | SyntaxKind::ContinueExpr
                | SyntaxKind::BreakExpr
                | SyntaxKind::WhileExpr
        )
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let result = match syntax.kind() {
            SyntaxKind::BinaryExpr => Self::BinaryExpr(BinaryExpr { syntax }),
            SyntaxKind::Literal => Self::Literal(Literal { syntax }),
            SyntaxKind::ParenExpr => Self::ParenExpr(ParenExpr { syntax }),
            SyntaxKind::UnaryExpr => Self::UnaryExpr(UnaryExpr { syntax }),
            SyntaxKind::PathExpr => Self::PathExpr(PathExpr { syntax }),
            SyntaxKind::CallExpr => Self::CallExpr(CallExpr { syntax }),
            SyntaxKind::BlockExpr => Self::BlockExpr(BlockExpr { syntax }),
            SyntaxKind::IfExpr => Self::IfExpr(IfExpr { syntax }),
            SyntaxKind::ReturnExpr => Self::ReturnExpr(ReturnExpr { syntax }),
            SyntaxKind::LoopExpr => Self::LoopExpr(LoopExpr { syntax }),
            SyntaxKind::ContinueExpr => Self::ContinueExpr(ContinueExpr { syntax }),
            SyntaxKind::BreakExpr => Self::BreakExpr(BreakExpr { syntax }),
            SyntaxKind::WhileExpr => Self::WhileExpr(WhileExpr { syntax }),
            _ => return None,
        };

        Some(result)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Expr::BinaryExpr(it) => it.syntax(),
            Expr::Literal(it) => it.syntax(),
            Expr::ParenExpr(it) => it.syntax(),
            Expr::UnaryExpr(it) => it.syntax(),
            Expr::PathExpr(it) => it.syntax(),
            Expr::CallExpr(it) => it.syntax(),
            Expr::BlockExpr(it) => it.syntax(),
            Expr::IfExpr(it) => it.syntax(),
            Expr::ReturnExpr(it) => it.syntax(),
            Expr::LoopExpr(it) => it.syntax(),
            Expr::ContinueExpr(it) => it.syntax(),
            Expr::BreakExpr(it) => it.syntax(),
            Expr::WhileExpr(it) => it.syntax(),
        }
    }
}

/// ステートメントノード
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    /// 変数定義
    VariableDef(VariableDef),
    /// 式ステートメント
    ExprStmt(ExprStmt),
    /// アイテム
    Item(Item),
}
impl Ast for Stmt {}
impl AstNode for Stmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, SyntaxKind::VariableDef | SyntaxKind::ExprStmt) || Item::can_cast(kind)
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let result = match syntax.kind() {
            SyntaxKind::VariableDef => Self::VariableDef(VariableDef { syntax }),
            SyntaxKind::ExprStmt => Self::ExprStmt(ExprStmt::cast(syntax)?),
            _ => {
                if let Some(item) = Item::cast(syntax) {
                    return Some(Stmt::Item(item));
                }
                return None;
            }
        };

        Some(result)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Stmt::VariableDef(it) => it.syntax(),
            Stmt::ExprStmt(it) => it.syntax(),
            Stmt::Item(it) => it.syntax(),
        }
    }
}

/// アイテムノード
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Item {
    /// 関数定義
    FunctionDef(FunctionDef),
    /// モジュール
    Module(Module),
    /// `use`アイテム
    Use(Use),
}
impl Ast for Item {}
impl AstNode for Item {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::FunctionDef | SyntaxKind::Module | SyntaxKind::Use
        )
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let result = match syntax.kind() {
            SyntaxKind::FunctionDef => Self::FunctionDef(FunctionDef { syntax }),
            SyntaxKind::Module => Self::Module(Module { syntax }),
            SyntaxKind::Use => Self::Use(Use { syntax }),
            _ => return None,
        };

        Some(result)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Item::FunctionDef(it) => it.syntax(),
            Item::Module(it) => it.syntax(),
            Item::Use(it) => it.syntax(),
        }
    }
}

def_ast_node!(
    /// 二項演算式のASTノード
    BinaryExpr
);
impl BinaryExpr {
    /// 左辺に位置する式ノードを返します。
    pub fn lhs(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }

    /// 右辺に位置する式ノードを返します。
    pub fn rhs(&self) -> Option<Expr> {
        ast_node::children_nodes(self).nth(1)
    }

    /// 二項演算子に位置するトークンを返します。
    pub fn op(&self) -> Option<tokens::BinaryOp> {
        ast_node::child_token(self)
    }
}

/// リテラルの種類
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    /// 整数リテラル
    Integer(tokens::Integer),
    /// 文字列リテラル
    String(tokens::String),
    /// 文字リテラル
    Char(tokens::Char),
    /// 真偽値リテラル
    Bool(tokens::Bool),
}

def_ast_node!(Literal);
impl Literal {
    fn token(&self) -> SyntaxToken {
        self.syntax.first_token().unwrap()
    }

    /// リテラルの種類を返します。
    pub fn kind(&self) -> LiteralKind {
        let token = self.token();
        if let Some(t) = tokens::Integer::cast(token.clone()) {
            LiteralKind::Integer(t)
        } else if let Some(t) = tokens::String::cast(token.clone()) {
            LiteralKind::String(t)
        } else if let Some(t) = tokens::Char::cast(token.clone()) {
            LiteralKind::Char(t)
        } else if let Some(t) = tokens::Bool::cast(token) {
            LiteralKind::Bool(t)
        } else {
            panic!("unknown literal kind");
        }
    }
}

def_ast_node!(
    /// 括弧式のASTノード
    ParenExpr
);
impl ParenExpr {
    /// 括弧式の中に位置する式ノードを返します。
    pub fn expr(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }
}

def_ast_node!(
    /// 単項演算式のASTノード
    UnaryExpr
);
impl UnaryExpr {
    /// 単項演算子の後に位置する式ノードを返します。
    pub fn expr(&self) -> Option<Expr> {
        self.syntax.children().find_map(Expr::cast)
    }

    /// 単項演算子に位置するトークンを返します。
    pub fn op(&self) -> Option<tokens::UnaryOp> {
        ast_node::child_token(self)
    }
}

def_ast_node!(
    /// パス式のASTノード
    PathExpr
);
impl PathExpr {
    /// パスノードを返します。
    pub fn path(&self) -> Option<Path> {
        ast_node::child_node(self)
    }
}

def_ast_node!(
    /// パスのASTノード
    Path
);
impl Path {
    /// パスを構成するセグメントの一覧を返します。
    pub fn segments(&self) -> impl Iterator<Item = PathSegment> {
        ast_node::children_nodes(self)
    }
}

def_ast_node!(
    /// パスのセグメントのASTノード
    PathSegment
);
impl PathSegment {
    /// パスのセグメントの名前に位置するトークンを返します。
    pub fn name(&self) -> Option<tokens::Ident> {
        ast_node::child_token(self)
    }
}

def_ast_node!(
    /// ブロック式のASTノード
    BlockExpr
);
impl BlockExpr {
    /// ブロック式の中に位置するステートメントの一覧を返します。
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> {
        ast_node::children_nodes(self)
    }
}

def_ast_node!(
    /// `if`式のASTノード
    IfExpr
);
impl IfExpr {
    /// 条件に位置する式ノードを返します。
    pub fn condition(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }

    /// `if`式の本体に位置する式ノードを返します。
    pub fn then_branch(&self) -> Option<BlockExpr> {
        self.children_after_condition().next()
    }

    /// `else`節に位置する式ノードを返します。
    pub fn else_branch(&self) -> Option<BlockExpr> {
        self.children_after_condition().nth(1)
    }

    /// 条件の後に位置する子ノードを返します。
    fn children_after_condition<N: AstNode>(&self) -> impl Iterator<Item = N> {
        self.syntax().children().skip(1).filter_map(N::cast)
    }
}

def_ast_node!(
    /// `return`式のASTノード
    ReturnExpr
);
impl ReturnExpr {
    /// `return`式の値に位置する式ノードを返します。
    pub fn value(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }
}

def_ast_node!(
    /// `loop`式のASTノード
    LoopExpr
);
impl LoopExpr {
    /// ループの本体に位置する式ノードを返します。
    pub fn body(&self) -> Option<BlockExpr> {
        ast_node::child_node(self)
    }
}

def_ast_node!(
    /// `continue`式のASTノード
    ContinueExpr
);

def_ast_node!(
    /// `break`式のASTノード
    BreakExpr
);
impl BreakExpr {
    /// `break`式の値に位置する式ノードを返します。
    pub fn value(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }
}

def_ast_node!(
    /// `while`式のASTノード
    WhileExpr
);
impl WhileExpr {
    /// 条件に位置する式ノードを返します。
    pub fn condition(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }

    /// ループの本体に位置する式ノードを返します。
    pub fn body(&self) -> Option<BlockExpr> {
        self.syntax().children().skip(1).find_map(BlockExpr::cast)
    }
}

def_ast_node!(
    /// 関数定義のASTノード
    FunctionDef
);
impl FunctionDef {
    /// 関数のパラメータのリストに位置するパラメータリストノードを返します。
    pub fn params(&self) -> Option<ParamList> {
        ast_node::child_node(self)
    }

    /// 関数の名前に位置するトークンを返します。
    pub fn name(&self) -> Option<tokens::Ident> {
        ast_node::child_token(self)
    }

    /// 関数の本体に位置する式ノードを返します。
    pub fn body(&self) -> Option<BlockExpr> {
        ast_node::child_node(self)
    }

    /// 関数の戻り値の型に位置する戻り値の型ノードを返します。
    pub fn return_type(&self) -> Option<ReturnType> {
        ast_node::child_node(self)
    }
}

def_ast_node!(
    /// 関数のパラメータのリストのASTノード
    ParamList
);
impl ParamList {
    /// パラメータの一覧を返します。
    pub fn params(&self) -> impl Iterator<Item = Param> {
        ast_node::children_nodes(self)
    }
}

def_ast_node!(
    /// 関数のパラメータのASTノード
    Param
);
impl Param {
    /// パラメータの名前に位置するトークンを返します。
    pub fn name(&self) -> Option<tokens::Ident> {
        ast_node::child_token(self)
    }

    /// パラメータの型に位置する型ノードを返します。
    pub fn ty(&self) -> Option<Type> {
        ast_node::child_node(self)
    }
}

def_ast_node!(
    /// 関数の戻り値の型のASTノード
    ReturnType
);
impl ReturnType {
    /// 関数の戻り値の型に位置する型ノードを返します。
    pub fn ty(&self) -> Option<Type> {
        ast_node::child_node(self)
    }
}

def_ast_node!(
    /// 型のASTノード
    Type
);
impl Type {
    /// 型の名前に位置するトークンを返します。
    pub fn ty(&self) -> Option<tokens::Ident> {
        ast_node::child_token(self)
    }
}

def_ast_node!(
    /// 関数呼び出しのASTノード
    CallExpr
);
impl CallExpr {
    /// 関数呼び出しの対象に位置する式ノードを返します。
    pub fn callee(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }

    /// 関数呼び出しの引数のリストに位置する引数リストノードを返します。
    pub fn args(&self) -> Option<ArgList> {
        ast_node::child_node(self)
    }
}

def_ast_node!(
    /// 関数呼び出しの引数のリストのASTノード
    ArgList
);
impl ArgList {
    /// 引数の一覧を返します。
    pub fn args(&self) -> impl Iterator<Item = Arg> {
        ast_node::children_nodes(self)
    }
}

def_ast_node!(
    /// 関数呼び出しの引数のASTノード
    Arg
);
impl Arg {
    /// 引数の値に位置する式ノードを返します。
    pub fn expr(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }
}

def_ast_node!(
    /// モジュールのASTノード
    Module
);
impl Module {
    /// モジュールの名前に位置するトークンを返します。
    pub fn name(&self) -> Option<tokens::Ident> {
        ast_node::child_token(self)
    }

    /// モジュールのアイテム一覧に位置するアイテム一覧ノードを返します。
    pub fn items(&self) -> Option<ItemList> {
        ast_node::child_node(self)
    }
}

def_ast_node!(
    /// アイテム一覧を表すASTノード
    ItemList
);
impl ItemList {
    /// アイテムの一覧を返します。
    pub fn items(&self) -> impl Iterator<Item = Item> {
        ast_node::children_nodes(self)
    }
}

def_ast_node!(
    /// `use`アイテムのASTノード
    Use
);
impl Use {
    /// パスに位置するパスノードを返します。
    pub fn path(&self) -> Option<Path> {
        ast_node::child_node(self)
    }
}
