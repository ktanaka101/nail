//! HIRの入力を定義します。
//!
//! HIRでは、ファイルシステムに依存しないようにします。
//! ファイルパスとファイル内容のパスを扱いますが、それをHIR以外のクレートから受け取ります。
//! HIRを使用する側は、事前にファイルシステムから読み込んでおく必要があります。

use std::collections::HashMap;

/// 入力元のNailファイルです。
#[salsa::input]
pub struct NailFile {
    /// ファイルパス
    #[reutrn_ref]
    pub file_path: std::path::PathBuf,

    /// ファイルの内容
    #[return_ref]
    pub contents: String,

    /// ルートファイルか
    pub root: bool,
}

/// ソースコードの元となる、ファイルを管理するデータベーストレイト
pub trait SourceDatabaseTrait {
    /// エントリポイントとなるファイルのIDを返す
    fn source_root(&self) -> NailFile;
    /// 登録済みの中からNailファイル情報を返す
    fn get_file(&self, path: &std::path::Path) -> Option<NailFile>;
}

/// テスト用のFixture
struct Fixture {
    source_root: NailFile,
    file_by_path: HashMap<std::path::PathBuf, NailFile>,
}
impl Fixture {
    /// 入力元のソースコードを構成する文字列をパースします。
    fn parse(db: &dyn crate::HirMasterDatabase, fixture: &str) -> Self {
        let fixture = fixture.trim();
        if !fixture.starts_with("//- ") {
            panic!("fixture must start with `//- `");
        }

        let mut file_by_path = HashMap::<std::path::PathBuf, NailFile>::new();

        let mut lines = fixture.lines().map(|line| line.trim());
        let mut current_file_path: Option<std::path::PathBuf> = None;
        let mut current_file_contents = String::new();

        loop {
            match lines.next() {
                Some(line) => {
                    if Self::is_file_path_line(line) {
                        // ファイルパス行が見つかったら、そこまでの情報で現在のファイルを登録します。
                        // 1行目の場合は`file_path`が`None`になので、登録は行われません。
                        if let Some(file_path) = current_file_path {
                            Self::register_file(
                                db,
                                file_path,
                                current_file_contents,
                                &mut file_by_path,
                            );
                        }

                        current_file_path =
                            Some(std::path::PathBuf::from(Self::take_file_path(line)));
                        current_file_contents = String::new();
                    } else {
                        current_file_contents.push_str(line);
                        current_file_contents.push('\n');
                    }
                }
                None => {
                    if let Some(current_file_path) = current_file_path {
                        if !file_by_path.contains_key(&current_file_path) {
                            Self::register_file(
                                db,
                                current_file_path,
                                current_file_contents,
                                &mut file_by_path,
                            );
                        }
                    }

                    break;
                }
            }
        }

        let source_root: Option<NailFile> = file_by_path
            .values()
            .find(|file| file.file_path(db) == std::path::Path::new("/main.nail"))
            .cloned();
        let Some(source_root) = source_root else {
            panic!("source root is not found. need: `/main.nail`")
        };

        Self {
            source_root,
            file_by_path,
        }
    }

    fn register_file(
        db: &dyn crate::HirMasterDatabase,
        file_path: std::path::PathBuf,
        file_contents: String,
        file_by_path: &mut HashMap<std::path::PathBuf, NailFile>,
    ) -> NailFile {
        if file_by_path.get(&file_path).is_some() {
            panic!("duplicate file path: {}", file_path.to_string_lossy());
        }

        let file = if file_path.to_str().unwrap() == "/main.nail" {
            NailFile::new(db, file_path, file_contents, true)
        } else {
            NailFile::new(db, file_path, file_contents, false)
        };
        file_by_path.insert(file.file_path(db), file);

        file
    }

    /// 行がファイルパス行かどうかを判定します。
    fn is_file_path_line(line: &str) -> bool {
        line.starts_with("//- ")
    }

    /// 行から、ファイルパスを取り出します。
    fn take_file_path(line: &str) -> &str {
        line.trim_start_matches("//- ")
    }
}

/// テスト用のFixtureを元に、ソースコードを管理するデータベース
///
/// ファイルシステムからソースコードを読み込むわけではありません。
/// 単純な文字列リテラルをソースコードとして扱います。
/// そのため、ファイルシステムを使用する代わりにpanicします。
pub struct FixtureDatabase {
    source_root: NailFile,
    file_by_path: HashMap<std::path::PathBuf, NailFile>,
}
impl FixtureDatabase {
    /// 入力元のソースコードを構成する文字列をパースして、データベースを構築します。
    ///
    /// # Example
    ///
    /// ```
    /// // `//- ファイル名`という記法で、その行以降の行は、そのファイルのソースコードとして扱われます。
    /// // `//- /main.nail`というファイル名で始まる行は、ソースコードのルートファイルとして扱われます。
    /// // この例では、/main.nailというエントリポイントと、/foo.nailというnailファイルが存在します。
    /// // /main.nailから、/foo.nailのbar関数が呼び出されています。
    /// use hir::{FixtureDatabase, TestingDatabase};
    /// let db = TestingDatabase::default();
    /// let fixture_db = FixtureDatabase::new(&db, r#"
    ///    //- /main.nail
    ///    mod foo;
    ///    fn main() {
    ///      foo::bar();
    ///    }
    ///    //- /foo.nail
    ///    pub fn bar() -> i32 {
    ///      10
    ///    }
    /// "#);
    pub fn new(db: &dyn crate::HirMasterDatabase, fixture: &str) -> Self {
        let fixture = Fixture::parse(db, fixture);

        Self {
            source_root: fixture.source_root,
            file_by_path: fixture.file_by_path,
        }
    }
}
impl SourceDatabaseTrait for FixtureDatabase {
    fn source_root(&self) -> NailFile {
        self.source_root
    }

    fn get_file(&self, path: &std::path::Path) -> Option<NailFile> {
        self.file_by_path.get(path).copied()
    }
}

/// ファイルシステムを使用する、ソースコードを管理するデータベース
pub struct SourceDatabase {
    source_root: NailFile,
    file_by_path: HashMap<std::path::PathBuf, NailFile>,
}
impl SourceDatabase {
    /// エントリポイントのソースコードをパースして、データベースを構築します。
    ///
    /// # Arguments
    ///
    /// * `root_file` - ソースコードのルートファイル
    /// * `file_by_path` - ファイルパスとNailファイルのマップ。ルートファイルも含めてください。
    pub fn new(root_file: NailFile, file_by_path: HashMap<std::path::PathBuf, NailFile>) -> Self {
        Self {
            source_root: root_file,
            file_by_path,
        }
    }
}
impl SourceDatabaseTrait for SourceDatabase {
    fn source_root(&self) -> NailFile {
        self.source_root
    }

    fn get_file(&self, path: &std::path::Path) -> Option<NailFile> {
        self.file_by_path.get(path).copied()
    }
}
