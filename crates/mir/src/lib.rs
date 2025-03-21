//! MIRを構築するためのモジュールです。
//!
//! MIRはLLVM IRに変換しやすくするための中間表現です。
//! このモジュールでは、HIRとTyped HIRをMIRに変換するための機能を提供します。
//!
//! MIRに変換する際に以下を行います。
//! - 脱糖
//!   例えば、`for`式は`while`式に変換されます。
//!   脱糖することにより、MIRからLLVM IRへの変換が容易になります。
//! - 制御フローグラフの構築
//!   制御フローグラフは、基本ブロックとその間の制御フローからなります。
//!   例えば、`if`式は、`then`ブロックと`else`ブロックを持つ基本ブロックに変換されます。
//!   この構造は、LLVM IRの基本ブロックと制御フローに対応しており、LLVM IRへの変換が容易になります。
#![warn(missing_docs)]
#![feature(trait_upcasting)]

mod body;
/// MIRの構築結果をテストするためのモジュール
pub mod testing;

use std::collections::HashMap;

use body::FunctionLower;
use hir_ty::Monotype;
use la_arena::{Arena, Idx};

/// HIRとTyped HIRからMIRを構築する
pub fn lower_pods(
    db: &dyn hir_ty::HirTyMasterDatabase,
    pods: &hir::Pods,
    hir_ty_result: &hir_ty::TyLowerResult,
) -> LowerResult {
    // TODO: 全てのPodをMIRに変換する
    let pod: &hir::Pod = &pods.root_pod;

    let mut entry_point: Option<Idx<Body>> = None;
    let mut bodies = Arena::new();

    let function_id_by_hir_function = {
        let mut function_id_resolver = FunctionIdGenerator::new();
        let mut function_id_by_hir_function = HashMap::<hir::Function, FunctionId>::new();
        for (_, function) in pod.all_functions(db) {
            function_id_by_hir_function.insert(function, function_id_resolver.gen());
        }

        function_id_by_hir_function
    };

    let mut body_by_function = HashMap::<FunctionId, Idx<Body>>::new();
    let mut function_by_body = HashMap::<Idx<Body>, FunctionId>::new();
    for (hir_file, function) in pod.all_functions(db) {
        let lower = FunctionLower::new(
            db,
            hir_file,
            &pods.resolution_map,
            hir_ty_result,
            &function_id_by_hir_function,
            function,
        );
        let body = lower.lower();
        let body_idx = bodies.alloc(body);

        body_by_function.insert(
            *function_id_by_hir_function.get(&function).unwrap(),
            body_idx,
        );
        function_by_body.insert(
            body_idx,
            *function_id_by_hir_function.get(&function).unwrap(),
        );

        let name = function.name(db).text(db);
        if name == "main" {
            assert_eq!(entry_point, None);
            entry_point = Some(body_idx);
        }
    }

    LowerResult {
        entry_point,
        bodies,
        body_by_function,
        function_by_body,
    }
}

/// 関数ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(u32);

/// 関数IDを生成する
///
/// 生成されるIDは0から始まり、1ずつ増えていきます。
struct FunctionIdGenerator {
    id: u32,
}
impl FunctionIdGenerator {
    fn new() -> Self {
        Self { id: 0 }
    }

    fn gen(&mut self) -> FunctionId {
        let id = self.id;
        self.id += 1;

        FunctionId(id)
    }
}

/// MIRの構築結果
#[derive(Debug)]
pub struct LowerResult {
    entry_point: Option<Idx<Body>>,
    bodies: Arena<Body>,
    body_by_function: HashMap<FunctionId, Idx<Body>>,
    function_by_body: HashMap<Idx<Body>, FunctionId>,
}

impl LowerResult {
    /// エントリポイントのボディを返す
    pub fn entry_point(&self) -> Option<&Body> {
        self.entry_point.map(|idx| &self.bodies[idx])
    }

    /// エントリポイントの関数IDを返す
    pub fn function_id_of_entry_point(&self) -> Option<FunctionId> {
        self.entry_point.map(|idx| self.function_by_body[&idx])
    }

    /// 関数IDからボディを返す
    pub fn body_by_function_id(&self, function_id: FunctionId) -> &Body {
        let body_idx = self.body_by_function.get(&function_id).unwrap();
        &self.bodies[*body_idx]
    }

    /// ボディから関数IDを返す
    pub fn function_id_by_body_idx(&self, idx: Idx<Body>) -> FunctionId {
        self.function_by_body[&idx]
    }

    /// (ボディインデックス, ボディ)のイテレータを返す
    pub fn ref_bodies(&self) -> impl Iterator<Item = (Idx<Body>, &Body)> {
        self.bodies.iter()
    }
}

/// 関数のボディ
#[derive(Debug)]
pub struct Body {
    /// 関数のパス
    pub path: hir::Path,
    /// 関数名
    pub name: hir::Name,
    /// 関数のパラメータ一覧
    pub params: Arena<Param>,
    /// 関数の戻り値
    pub return_local: LocalIdx,
    /// 関数内のローカル変数一覧
    pub locals: Arena<Local>,
    /// 関数内の基本ブロック一覧
    pub blocks: Arena<BasicBlock>,
}
impl Body {
    /// 関数のシグネチャを返す
    pub fn signature(&self) -> Signature {
        let params = self.params.iter().map(|(idx, _param)| idx).collect();
        Signature {
            params,
            return_value: self.return_local,
        }
    }

    /// Return type of the function
    pub fn return_type(&self) -> Monotype {
        self.locals[self.return_local].ty
    }
}

/// 関数のシグネチャ
#[derive(Debug)]
pub struct Signature {
    /// パラメータ一覧
    pub params: Vec<ParamIdx>,
    /// 戻り値
    pub return_value: LocalIdx,
}

/// 関数のパラメータ
#[derive(Debug)]
pub struct Param {
    /// パラメータの型
    pub ty: Monotype,
    /// パラメータのローカル変数のインデックス
    pub idx: u64,
    /// パラメータの位置
    pub pos: u32,
}
/// パラメータの保持インデックス
pub type ParamIdx = Idx<Param>;

/// 関数内のローカル変数
#[derive(Debug)]
pub struct Local {
    /// ローカル変数の型
    pub ty: Monotype,
    /// ローカル変数のインデックス
    pub idx: u64,
}
/// ローカル変数の保持インデックス
pub type LocalIdx = Idx<Local>;

/// 基本ブロック
#[derive(Debug)]
pub struct BasicBlock {
    /// 基本ブロックの種類
    pub kind: BasicBlockKind,
    /// 基本ブロック内の文一覧
    pub statements: Vec<Statement>,
    /// 基本ブロックの終端
    pub termination: Option<Termination>,
    /// 基本ブロックのインデックス
    idx: u64,
}

impl BasicBlock {
    /// 基本ブロックの名前を返す
    pub fn name(&self) -> String {
        match self.kind {
            crate::BasicBlockKind::Entry => "entry".to_string(),
            crate::BasicBlockKind::Exit => "exit".to_string(),
            crate::BasicBlockKind::Standard => {
                format!("bb{}", self.idx)
            }
            crate::BasicBlockKind::Then => {
                format!("then{}", self.idx)
            }
            crate::BasicBlockKind::Else => {
                format!("else{}", self.idx)
            }
            crate::BasicBlockKind::Loop => {
                format!("loop{}", self.idx)
            }
        }
    }

    fn new_entry_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Entry,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_exit_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Exit,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_then_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Then,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_else_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Else,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_loop_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Loop,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_standard_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Standard,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn add_statement(&mut self, stmt: Statement) {
        self.statements.push(stmt);
    }
}

/// 基本ブロックの保持インデックス
pub type BasicBlockIdx = Idx<BasicBlock>;

struct AllocatedSwitchBB {
    then_bb_idx: Idx<BasicBlock>,
    else_bb_idx: Idx<BasicBlock>,
}

/// 値の位置を指す
/// 参照のようなもの
#[derive(Debug, Clone, Copy)]
pub struct Place {
    /// 値の位置の種類
    pub local: PlaceKind,
    /// 値の位置からの相対的な位置
    pub projection: Option<Projection>,
}

/// 値の位置の種類
#[derive(Debug, Clone, Copy)]
pub enum PlaceKind {
    /// 関数パラメータ
    Param(Idx<Param>),
    /// ローカル変数
    Local(Idx<Local>),
}

/// 相対的に表される位置
#[derive(Debug, Clone, Copy)]
pub enum Projection {
    /// フィールド
    Field {
        /// フィールドのインデックス
        idx: u32,
        /// Field type
        ty: Monotype,
        /// フィールド名
        name: hir::Name,
    },
}

/// 値
#[derive(Debug)]
pub enum Value {
    /// オペランド(値)
    Operand(Operand),
    /// 二項演算
    BinaryOp {
        /// 二項演算子
        op: BinaryOp,
        /// 左辺
        left: Operand,
        /// 右辺
        right: Operand,
    },
    /// 単項演算
    UnaryOp {
        /// 単項演算子
        op: UnaryOp,
        /// 式
        expr: Operand,
    },
    /// 構造体などの集合体の作成(今後タプルや配列も)
    Aggregate {
        /// 集合体の種類
        kind: AggregateKind,
        /// 集合のオペランド一覧
        operands: Vec<Operand>,
    },
}

/// 集合体の種類
#[derive(Debug)]
pub enum AggregateKind {
    /// 構造体
    Struct(hir::Struct),
}

/// 二項演算子
#[derive(Debug)]
pub enum BinaryOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `==`
    Equal,
    /// `!=`
    NotEq,
    /// `>`
    GreaterThan,
    /// `<`
    LessThan,
    /// `>=`
    GtEq,
    /// `<=`
    LtEq,
}

/// 単項演算子
#[derive(Debug)]
pub enum UnaryOp {
    /// `-`
    Neg,
    /// `!`
    Not,
}

/// オペランド
#[derive(Debug)]
pub enum Operand {
    /// 値の位置
    Place(Place),
    /// 定数
    Constant(Constant),
}
impl From<Operand> for Value {
    fn from(value: Operand) -> Self {
        Self::Operand(value)
    }
}

#[derive(Debug)]
enum LoweredExpr {
    Return,
    Break,
    Operand(Operand),
}

#[derive(Debug)]
enum LoweredStmt {
    Return,
    Break,
    Unit,
}

/// 定数
#[derive(Debug)]
pub enum Constant {
    /// 整数
    Integer(u64),
    /// 真偽値
    Boolean(bool),
    /// 文字列
    String(String),
    /// 単値
    Unit,
    /// 空構造体
    StructUnit(hir::Struct),
}

/// ステートメント
#[derive(Debug)]
pub enum Statement {
    /// 値の代入
    Assign {
        /// 代入先
        place: Place,
        /// 代入する値
        value: Value,
    },
}

/// 終端
#[derive(Debug)]
pub enum Termination {
    /// 戻り値
    Return(Idx<Local>),
    /// ジャンプ
    Goto(Idx<BasicBlock>),
    /// 条件分岐
    Switch {
        /// 条件
        condition: Place,
        /// 条件が真の場合にジャンプする先の基本ブロック
        then_bb: Idx<BasicBlock>,
        /// 条件が偽の場合にジャンプする先の基本ブロック
        else_bb: Idx<BasicBlock>,
    },
    /// 関数呼び出し
    Call {
        /// 呼び出し対象の関数
        function: FunctionId,
        /// 引数
        args: Vec<Operand>,
        /// 戻り値の代入先
        destination: Place,
        /// 呼び出し後にジャンプする先の基本ブロック
        target: Idx<BasicBlock>,
    },
}

/// 基本ブロックの種類
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BasicBlockKind {
    /// 関数の開始を表す基本ブロック
    Entry,
    /// 関数の終了を表す基本ブロック
    Exit,
    /// 標準的な基本ブロック
    Standard,
    /// 条件分岐の真の場合の基本ブロック
    Then,
    /// 条件分岐の偽の場合の基本ブロック
    Else,
    /// ループの基本ブロック
    Loop,
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::lower_pods;

    fn check_pod_start_with_root_file(fixture: &str, expect: Expect) {
        let db: hir_ty::TestingDatabase = hir_ty::TestingDatabase::default();
        let source_db = hir::FixtureDatabase::new(&db, fixture);

        let pods = hir::parse_pods(&db, &source_db);
        let ty_hir_result = hir_ty::lower_pods(&db, &pods);

        let mir_result = lower_pods(&db, &pods, &ty_hir_result);

        expect.assert_eq(&crate::testing::Pretty::new(&db, &mir_result).format());
    }

    fn check_in_root_file(fixture: &str, expect: Expect) {
        let mut fixture = fixture.to_string();
        fixture.insert_str(0, "//- /main.nail\n");

        check_pod_start_with_root_file(&fixture, expect);
    }

    #[test]
    fn test_add_number() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    10 + 20
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = add(const 10, const 20)
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    let a = 100;
                    let b = 10 + 20;

                    10 + 20 + a + b
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int
                    let _2: int
                    let _3: int
                    let _4: int
                    let _5: int
                    let _6: int

                    entry: {
                        _1 = const 100
                        _3 = add(const 10, const 20)
                        _2 = _3
                        _4 = add(const 10, const 20)
                        _5 = add(_4, _1)
                        _6 = add(_5, _3)
                        _0 = _6
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_sub_number() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    10 - 20
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = sub(const 10, const 20)
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_mul_number() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    10 * 20
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = mul(const 10, const 20)
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_div_number() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    10 / 20
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = div(const 10, const 20)
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_equal_number() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    10 == 20
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> bool {
                    let _0: bool
                    let _1: bool

                    entry: {
                        _1 = equal(const 10, const 20)
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_equal_bool() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    let a = true;
                    let b = true;
                    a == b
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> bool {
                    let _0: bool
                    let _1: bool
                    let _2: bool
                    let _3: bool

                    entry: {
                        _1 = const true
                        _2 = const true
                        _3 = equal(_1, _2)
                        _0 = _3
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_not_equal_number() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    10 != 20
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> bool {
                    let _0: bool
                    let _1: bool

                    entry: {
                        _1 = not_equal(const 10, const 20)
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_ord_number() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    let a = 1;
                    let b = 2;
                    a > b;
                    a < b;
                    b > a;
                    b < a;
                    a >= b;
                    a <= b;
                    b >= a;
                    b <= a;
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> bool {
                    let _0: bool
                    let _1: int
                    let _2: int
                    let _3: bool
                    let _4: bool
                    let _5: bool
                    let _6: bool
                    let _7: bool
                    let _8: bool
                    let _9: bool
                    let _10: bool

                    entry: {
                        _1 = const 1
                        _2 = const 2
                        _3 = greater_than(_1, _2)
                        _4 = less_than(_1, _2)
                        _5 = greater_than(_2, _1)
                        _6 = less_than(_2, _1)
                        _7 = gteq(_1, _2)
                        _8 = lteq(_1, _2)
                        _9 = gteq(_2, _1)
                        _10 = lteq(_2, _1)
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_assign() {
        check_in_root_file(
            r#"
                fn main() {
                    let a = 1;
                    a = 10;
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> () {
                    let _0: ()
                    let _1: int

                    entry: {
                        _1 = const 1
                        _1 = const 10
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_return_assigned() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    let a = 10;
                    a = 20;

                    a
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = const 10
                        _1 = const 20
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_assign_returns_unit() {
        check_in_root_file(
            r#"
                fn main() {
                    let a = 1;
                    let b = a = 10;
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> () {
                    let _0: ()
                    let _1: int
                    let _2: ()

                    entry: {
                        _1 = const 1
                        _1 = const 10
                        _2 = const ()
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_negative_number() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    let a = -10;
                    let b = 20;
                    a == -b
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> bool {
                    let _0: bool
                    let _1: int
                    let _2: int
                    let _3: int
                    let _4: int
                    let _5: bool

                    entry: {
                        _2 = negative(const 10)
                        _1 = _2
                        _3 = const 20
                        _4 = negative(_3)
                        _5 = equal(_2, _4)
                        _0 = _5
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_not_bool() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    let a = !true;
                    let b = false;
                    a == !b
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> bool {
                    let _0: bool
                    let _1: bool
                    let _2: bool
                    let _3: bool
                    let _4: bool
                    let _5: bool

                    entry: {
                        _2 = not(const true)
                        _1 = _2
                        _3 = const false
                        _4 = not(_3)
                        _5 = equal(_2, _4)
                        _0 = _5
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_main_return_int() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    10
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int

                    entry: {
                        _0 = const 10
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_main_return_unit() {
        check_in_root_file(
            r#"
                fn main() {
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> () {
                    let _0: ()

                    entry: {
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() {
                    10;
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> () {
                    let _0: ()

                    entry: {
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_main_return_bool() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    true
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> bool {
                    let _0: bool

                    entry: {
                        _0 = const true
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_main_return_string() {
        check_in_root_file(
            r#"
                fn main() -> string {
                    "aaa"
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> string {
                    let _0: string

                    entry: {
                        _0 = const "aaa"
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_let() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    let x = 10;
                    x
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = const 10
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_block_resolves_to_tail() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    {
                        let x = 10;
                        x
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = const 10
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_block_without_tail_resolves_to_unit() {
        check_in_root_file(
            r#"
                fn main() {
                    {
                        let x = 10;
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> () {
                    let _0: ()
                    let _1: int

                    entry: {
                        _1 = const 10
                        _0 = const ()
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_return() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    return true;
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> bool {
                    let _0: bool

                    entry: {
                        _0 = const true
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> bool {
                    return true
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> bool {
                    let _0: bool

                    entry: {
                        _0 = const true
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    return 10;
                    20
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int

                    entry: {
                        _0 = const 10
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_return_in_block() {
        // return by statement in block
        check_in_root_file(
            r#"
                fn main() -> bool {
                    let c = {
                        let a = true;
                        return a;
                        let b = false;
                        b
                    };
                    c
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> bool {
                    let _0: bool
                    let _1: !
                    let _2: bool

                    entry: {
                        _2 = const true
                        _0 = _2
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        // return by tail in block
        check_in_root_file(
            r#"
                fn main() -> int {
                    let c = {
                        return 10;
                    };
                    c
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: !

                    entry: {
                        _0 = const 10
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_return_in_switch() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        return 10;
                    } else {
                        return 20;
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: !

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                    }

                    then0: {
                        _0 = const 10
                        goto -> exit
                    }

                    else0: {
                        _0 = const 20
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn return_value_in_else_branch() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        10
                    } else {
                        return 20;
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }

                    then0: {
                        _2 = const 10
                        goto -> bb0
                    }

                    else0: {
                        _0 = const 20
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn return_value_in_then_branch() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        return 10;
                    } else {
                        20
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }

                    then0: {
                        _0 = const 10
                        goto -> exit
                    }

                    else0: {
                        _2 = const 20
                        goto -> bb0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_switch() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        10
                    } else {
                        20
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }

                    then0: {
                        _2 = const 10
                        goto -> bb0
                    }

                    else0: {
                        _2 = const 20
                        goto -> bb0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_switch_conditional_early_return() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    let a = 10;
                    if true {
                        return 20;
                    }

                    a
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int
                    let _2: bool
                    let _3: !

                    entry: {
                        _1 = const 10
                        _2 = const true
                        switch(_2) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }

                    then0: {
                        _0 = const 20
                        goto -> exit
                    }

                    else0: {
                        _3 = const ()
                        goto -> bb0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_switch_only_then_branch() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    let a = 10;
                    if true {
                        20
                    }

                    a
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int
                    let _2: bool
                    let _3: int

                    entry: {
                        _1 = const 10
                        _2 = const true
                        switch(_2) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }

                    then0: {
                        _3 = const 20
                        goto -> bb0
                    }

                    else0: {
                        _3 = const ()
                        goto -> bb0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_statements_in_switch() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        let b = 20;
                        b
                    } else {
                        let c = 20;
                        c
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int
                    let _3: int
                    let _4: int

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }

                    then0: {
                        _3 = const 20
                        _2 = _3
                        goto -> bb0
                    }

                    else0: {
                        _4 = const 20
                        _2 = _4
                        goto -> bb0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn return_in_if() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    if false {
                        return 10;
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: !

                    entry: {
                        _1 = const false
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }

                    then0: {
                        _0 = const 10
                        goto -> exit
                    }

                    else0: {
                        _2 = const ()
                        goto -> bb0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    if false {
                        10
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int

                    entry: {
                        _1 = const false
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }

                    then0: {
                        _2 = const 10
                        goto -> bb0
                    }

                    else0: {
                        _2 = const ()
                        goto -> bb0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    if false {
                        10
                    } else {
                        return 20;
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int

                    entry: {
                        _1 = const false
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }

                    then0: {
                        _2 = const 10
                        goto -> bb0
                    }

                    else0: {
                        _0 = const 20
                        goto -> exit
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    if false {
                        return 10;
                    } else {
                        20
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int

                    entry: {
                        _1 = const false
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }

                    then0: {
                        _0 = const 10
                        goto -> exit
                    }

                    else0: {
                        _2 = const 20
                        goto -> bb0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    if false {
                        10
                    } else {
                        20
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int

                    entry: {
                        _1 = const false
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }

                    then0: {
                        _2 = const 10
                        goto -> bb0
                    }

                    else0: {
                        _2 = const 20
                        goto -> bb0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    if false {
                        return 10;
                    } else {
                        return 20;
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: !

                    entry: {
                        _1 = const false
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                    }

                    then0: {
                        _0 = const 10
                        goto -> exit
                    }

                    else0: {
                        _0 = const 20
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_call() {
        check_in_root_file(
            r#"
                fn aaa() -> int {
                    10
                }

                fn main() -> int {
                    aaa()
                }
            "#,
            expect![[r#"
                fn t_pod::aaa() -> int {
                    let _0: int

                    entry: {
                        _0 = const 10
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = aaa() -> [return: bb0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_call_with_args() {
        check_in_root_file(
            r#"
                fn aaa(x: int, y: int) -> int {
                    x + y
                }

                fn main() -> int {
                    aaa(10, 20)
                }
            "#,
            expect![[r#"
                fn t_pod::aaa(_1: int, _2: int) -> int {
                    let _0: int
                    let _3: int

                    entry: {
                        _3 = add(_1, _2)
                        _0 = _3
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = aaa(const 10, const 20) -> [return: bb0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn return_unit() {
        check_in_root_file(
            r#"
                fn main() {
                    return;
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> () {
                    let _0: ()

                    entry: {
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn infinite_loop() {
        check_in_root_file(
            r#"
                fn main() {
                    loop {
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> () {
                    let _0: ()
                    let _1: !

                    entry: {
                        goto -> loop0
                    }

                    exit: {
                        return _0
                    }

                    loop0: {
                        goto -> loop0
                    }

                    bb1: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn break_in_loop() {
        check_in_root_file(
            r#"
                fn main() {
                    loop {
                        break;
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> () {
                    let _0: ()
                    let _1: ()

                    entry: {
                        goto -> loop0
                    }

                    exit: {
                        return _0
                    }

                    loop0: {
                        _1 = const ()
                        goto -> bb1
                    }

                    bb1: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn break_with_expr_in_loop() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        break 10;
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        goto -> loop0
                    }

                    exit: {
                        return _0
                    }

                    loop0: {
                        _1 = const 10
                        goto -> bb1
                    }

                    bb1: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn break_in_if() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        if false {
                            break 10;
                        }
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int
                    let _2: bool
                    let _3: !

                    entry: {
                        goto -> loop0
                    }

                    exit: {
                        return _0
                    }

                    loop0: {
                        _2 = const false
                        switch(_2) -> [true: then0, false: else0]
                    }

                    bb1: {
                        _0 = _1
                        goto -> exit
                    }

                    bb2: {
                        goto -> loop0
                    }

                    then0: {
                        _1 = const 10
                        goto -> bb1
                    }

                    else0: {
                        _3 = const ()
                        goto -> bb2
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() {
                    loop {
                        if false {
                            10;
                        }
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> () {
                    let _0: ()
                    let _1: !
                    let _2: bool
                    let _3: ()

                    entry: {
                        goto -> loop0
                    }

                    exit: {
                        return _0
                    }

                    loop0: {
                        _2 = const false
                        switch(_2) -> [true: then0, false: else0]
                    }

                    bb1: {
                        _0 = _1
                        goto -> exit
                    }

                    bb2: {
                        goto -> loop0
                    }

                    then0: {
                        goto -> bb2
                    }

                    else0: {
                        _3 = const ()
                        goto -> bb2
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        if false {
                            10
                        } else {
                            break 20;
                        }
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int
                    let _2: bool
                    let _3: int

                    entry: {
                        goto -> loop0
                    }

                    exit: {
                        return _0
                    }

                    loop0: {
                        _2 = const false
                        switch(_2) -> [true: then0, false: else0]
                    }

                    bb1: {
                        _0 = _1
                        goto -> exit
                    }

                    bb2: {
                        goto -> loop0
                    }

                    then0: {
                        _3 = const 10
                        goto -> bb2
                    }

                    else0: {
                        _1 = const 20
                        goto -> bb1
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        if false {
                            break 10;
                        } else {
                            20
                        }
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int
                    let _2: bool
                    let _3: int

                    entry: {
                        goto -> loop0
                    }

                    exit: {
                        return _0
                    }

                    loop0: {
                        _2 = const false
                        switch(_2) -> [true: then0, false: else0]
                    }

                    bb1: {
                        _0 = _1
                        goto -> exit
                    }

                    bb2: {
                        goto -> loop0
                    }

                    then0: {
                        _1 = const 10
                        goto -> bb1
                    }

                    else0: {
                        _3 = const 20
                        goto -> bb2
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        if false {
                            break 10;
                        } else {
                            break 20;
                        }
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int
                    let _2: bool
                    let _3: !

                    entry: {
                        goto -> loop0
                    }

                    exit: {
                        return _0
                    }

                    loop0: {
                        _2 = const false
                        switch(_2) -> [true: then0, false: else0]
                    }

                    bb1: {
                        _0 = _1
                        goto -> exit
                    }

                    bb2: {
                    }

                    then0: {
                        _1 = const 10
                        goto -> bb1
                    }

                    else0: {
                        _1 = const 20
                        goto -> bb1
                    }
                }
            "#]],
        );
    }

    #[test]
    fn break_and_return_in_if() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        if false {
                            return 10;
                        } else {
                            break 20;
                        }
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int
                    let _2: bool
                    let _3: !

                    entry: {
                        goto -> loop0
                    }

                    exit: {
                        return _0
                    }

                    loop0: {
                        _2 = const false
                        switch(_2) -> [true: then0, false: else0]
                    }

                    bb1: {
                        _0 = _1
                        goto -> exit
                    }

                    bb2: {
                    }

                    then0: {
                        _0 = const 10
                        goto -> exit
                    }

                    else0: {
                        _1 = const 20
                        goto -> bb1
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        if false {
                            break 10;
                        } else {
                            return 20;
                        }
                    }
                }            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int
                    let _2: bool
                    let _3: !

                    entry: {
                        goto -> loop0
                    }

                    exit: {
                        return _0
                    }

                    loop0: {
                        _2 = const false
                        switch(_2) -> [true: then0, false: else0]
                    }

                    bb1: {
                        _0 = _1
                        goto -> exit
                    }

                    bb2: {
                    }

                    then0: {
                        _1 = const 10
                        goto -> bb1
                    }

                    else0: {
                        _0 = const 20
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn continue_in_loop() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        continue;
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: !

                    entry: {
                        goto -> loop0
                    }

                    exit: {
                        return _0
                    }

                    loop0: {
                        goto -> loop0
                    }

                    bb1: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_while() {
        check_in_root_file(
            r#"
                fn main() {
                    let i = 1;
                    while i < 3 {
                        i = i + 1;
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> () {
                    let _0: ()
                    let _1: int
                    let _2: ()
                    let _3: bool
                    let _4: bool
                    let _5: ()
                    let _6: int

                    entry: {
                        _1 = const 1
                        goto -> loop0
                    }

                    exit: {
                        return _0
                    }

                    loop0: {
                        _4 = less_than(_1, const 3)
                        _3 = _4
                        switch(_3) -> [true: then0, false: else0]
                    }

                    bb1: {
                        _0 = _2
                        goto -> exit
                    }

                    bb2: {
                        goto -> loop0
                    }

                    then0: {
                        _6 = add(_1, const 1)
                        _1 = _6
                        goto -> bb2
                    }

                    else0: {
                        _2 = const ()
                        goto -> bb1
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_string_arg() {
        check_in_root_file(
            r#"
                fn aaa(x: string, y: string) -> string {
                    y
                }

                fn main() -> string {
                    aaa("aaa", "bbb")
                }
            "#,
            expect![[r#"
                fn t_pod::aaa(_1: string, _2: string) -> string {
                    let _0: string

                    entry: {
                        _0 = _2
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn t_pod::main() -> string {
                    let _0: string
                    let _1: string

                    entry: {
                        _1 = aaa(const "aaa", const "bbb") -> [return: bb0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_struct_arg() {
        check_in_root_file(
            r#"
                struct Point { x: int, y: string }
                fn foo(point: Point) -> Point {
                    point
                }

                fn main() -> Point {
                    foo(Point { x: 10, y: "aaa"))
                }
            "#,
            expect![[r#"
                fn t_pod::foo(_1: struct Point) -> struct Point {
                    let _0: struct Point

                    entry: {
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn t_pod::main() -> struct Point {
                    let _0: struct Point
                    let _1: struct Point
                    let _2: struct Point

                    entry: {
                        _1 = Point { x: const 10, y: const "aaa" }
                        _2 = foo(_1) -> [return: bb0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_struct_record_fields_order() {
        check_in_root_file(
            r#"
                struct Point { x: int, y: string }
                fn main() -> Point {
                    let point = Point { y: "aaa", x: 10 };
                    point
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> struct Point {
                    let _0: struct Point
                    let _1: struct Point
                    let _2: struct Point

                    entry: {
                        _2 = Point { x: const 10, y: const "aaa" }
                        _1 = _2
                        _0 = _2
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                struct Point { x: int, y: int }
                fn main() -> Point {
                    let point = Point { y: 10 + 20, x: 30 + 40 };
                    point
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> struct Point {
                    let _0: struct Point
                    let _1: struct Point
                    let _2: int
                    let _3: int
                    let _4: struct Point

                    entry: {
                        _2 = add(const 10, const 20)
                        _3 = add(const 30, const 40)
                        _4 = Point { x: _3, y: _2 }
                        _1 = _4
                        _0 = _4
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_call_binding() {
        check_in_root_file(
            r#"
                fn aaa(x: int, y: int) -> int {
                    x + y
                }

                fn main() -> int {
                    let a = 10;
                    let b = 20;
                    let c = aaa(a, b);
                    c
                }
            "#,
            expect![[r#"
                fn t_pod::aaa(_1: int, _2: int) -> int {
                    let _0: int
                    let _3: int

                    entry: {
                        _3 = add(_1, _2)
                        _0 = _3
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: int
                    let _2: int
                    let _3: int
                    let _4: int

                    entry: {
                        _1 = const 10
                        _2 = const 20
                        _4 = aaa(_1, _2) -> [return: bb0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _3 = _4
                        _0 = _3
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_modules() {
        check_in_root_file(
            r#"
                fn main() {
                    return;
                }
                mod module_aaa {
                    mod module_bbb {
                        fn function_aaa() -> int {
                            mod module_ccc {
                                fn function_bbb() -> int {
                                    10
                                }
                            }

                            20
                        }
                    }

                    fn function_ccc() -> int {
                        30
                    }
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> () {
                    let _0: ()

                    entry: {
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn t_pod::module_aaa::module_bbb::function_aaa() -> int {
                    let _0: int

                    entry: {
                        _0 = const 20
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn t_pod::module_aaa::module_bbb::function_aaa::module_ccc::function_bbb() -> int {
                    let _0: int

                    entry: {
                        _0 = const 10
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn t_pod::module_aaa::function_ccc() -> int {
                    let _0: int

                    entry: {
                        _0 = const 30
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn nested_outline_module() {
        check_pod_start_with_root_file(
            r#"
                //- /main.nail
                mod mod_aaa;

                fn main() -> integer {
                    mod_aaa::fn_aaa();
                    mod_aaa::mod_bbb::fn_bbb()
                }

                //- /mod_aaa.nail
                mod mod_bbb;
                fn fn_aaa() -> integer {
                    mod_bbb::fn_bbb()
                }

                //- /mod_aaa/mod_bbb.nail
                fn fn_bbb() -> integer {
                    10
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> unknown {
                    let _0: unknown
                    let _1: unknown
                    let _2: unknown

                    entry: {
                        _1 = fn_aaa() -> [return: bb0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _2 = fn_bbb() -> [return: bb1]
                    }

                    bb1: {
                        _0 = _2
                        goto -> exit
                    }
                }
                fn t_pod::mod_aaa::fn_aaa() -> unknown {
                    let _0: unknown
                    let _1: unknown

                    entry: {
                        _1 = fn_bbb() -> [return: bb0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }
                }
                fn t_pod::mod_aaa::mod_bbb::fn_bbb() -> unknown {
                    let _0: unknown

                    entry: {
                        _0 = const 10
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_record_field_expr() {
        check_in_root_file(
            r#"
                struct Point { x: int, y: string }
                fn main() -> int {
                    let point = Point { x: 10, y: "aaa" };
                    let a = point.x;
                    let b = point.y;

                    a
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: struct Point
                    let _2: struct Point
                    let _3: int
                    let _4: struct Point
                    let _5: string
                    let _6: struct Point

                    entry: {
                        _2 = Point { x: const 10, y: const "aaa" }
                        _1 = _2
                        _4 = _2
                        _3 = _4.x
                        _6 = _2
                        _5 = _6.y
                        _0 = _3
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_tuple_field_expr() {
        check_in_root_file(
            r#"
                struct Point(int, string)
                fn main() -> int {
                    let point = Point(10, "aaa");
                    let a = point.0;
                    let b = point.1;

                    a
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> int {
                    let _0: int
                    let _1: struct Point
                    let _2: struct Point
                    let _3: int
                    let _4: struct Point
                    let _5: string
                    let _6: struct Point

                    entry: {
                        _2 = Point(const 10, const "aaa")
                        _1 = _2
                        _4 = _2
                        _3 = _4.0
                        _6 = _2
                        _5 = _6.1
                        _0 = _3
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn init_struct() {
        check_in_root_file(
            r#"
                struct PointRecord { x: string, y: int }
                struct PointTuple(string, int);
                struct PointUnit;
                fn main() {
                    let point_record = PointRecord { x: "aaa", y: 10 };
                    let point_tuple = PointTuple("bbb", 20);
                    let u = PointUnit;
                }
            "#,
            expect![[r#"
                fn t_pod::main() -> () {
                    let _0: ()
                    let _1: struct PointRecord
                    let _2: struct PointRecord
                    let _3: struct PointTuple
                    let _4: struct PointTuple
                    let _5: struct PointUnit

                    entry: {
                        _2 = PointRecord { x: const "aaa", y: const 10 }
                        _1 = _2
                        _4 = PointTuple(const "bbb", const 20)
                        _3 = _4
                        _5 = const PointUnit
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }
}
