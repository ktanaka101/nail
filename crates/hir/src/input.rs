mod file_path_interner;

use std::collections::HashMap;

use self::file_path_interner::{FilePath, FilePathInterner};

/// ファイルを一意に識別するためのID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(FilePath);

/// ソースコードの元となる、ファイルを管理するデータベーストレイト
pub trait SourceDatabaseTrait {
    /// エントリポイントとなるファイルのIDを返す
    fn source_root(&self) -> FileId;
    /// ファイルパスからファイルIDを返す
    fn register_file_with_read(&mut self, path: &str) -> FileId;
    /// ファイルIDからファイル内容を返す
    fn content(&self, file_id: FileId) -> Option<&str>;
}

/// テスト用のFixture
struct Fixture {
    source_root: FileId,
    file_by_path: HashMap<FilePath, FileId>,
    path_by_file: HashMap<FileId, FilePath>,
    file_contents: HashMap<FileId, String>,
    file_path_interner: FilePathInterner,
}
impl Fixture {
    /// 入力元のソースコードを構成する文字列をパースします。
    fn parse(fixture: &str) -> Self {
        let fixture = fixture.trim();
        if !fixture.starts_with("//- ") {
            panic!("fixture must start with `//- `");
        }

        let mut file_by_path = HashMap::<FilePath, FileId>::new();
        let mut path_by_file = HashMap::<FileId, FilePath>::new();
        let mut file_path_interner = FilePathInterner::new();

        fn register_file_path(
            file_path: &str,
            file_by_path: &mut HashMap<FilePath, FileId>,
            path_by_file: &mut HashMap<FileId, FilePath>,
            file_path_interner: &mut FilePathInterner,
        ) -> FileId {
            if file_path_interner.get(file_path).is_some() {
                panic!("duplicate file path: {file_path}");
            }

            let file_path = file_path_interner.intern(file_path);
            let file_id = FileId(file_path);
            file_by_path.insert(file_path, file_id);
            path_by_file.insert(file_id, file_path);

            file_id
        }
        fn is_file_path_line(line: &str) -> bool {
            line.starts_with("//- ")
        }
        fn take_file_path(line: &str) -> &str {
            line.trim_start_matches("//- ")
        }

        let mut source_root: Option<FileId> = None;

        let mut lines = fixture.lines().map(|line| line.trim());
        let mut current_file = String::new();
        let mut current_file_id = {
            let first_line = lines.next().unwrap();
            let file_path = take_file_path(first_line);
            let file_id = register_file_path(
                file_path,
                &mut file_by_path,
                &mut path_by_file,
                &mut file_path_interner,
            );
            if file_path == "/main.nail" {
                source_root = Some(file_id);
            }

            file_id
        };

        let mut file_contents = HashMap::<FileId, String>::new();
        loop {
            match lines.next() {
                Some(line) => {
                    if is_file_path_line(line) {
                        file_contents.insert(current_file_id, current_file);

                        let file_path = take_file_path(line);
                        let file_id = register_file_path(
                            file_path,
                            &mut file_by_path,
                            &mut path_by_file,
                            &mut file_path_interner,
                        );
                        if file_path == "/main.nail" {
                            source_root = Some(file_id);
                        }
                        current_file_id = file_id;
                        current_file = String::new();
                    } else {
                        current_file.push_str(line);
                        current_file.push('\n');
                    }
                }
                None => {
                    file_contents.insert(current_file_id, current_file);
                    break;
                }
            }
        }

        let Some(source_root) = source_root else { panic!("source root is not found. need: `/main.nail`") };

        Self {
            source_root,
            file_by_path,
            path_by_file,
            file_contents,
            file_path_interner,
        }
    }
}

/// テスト用のFixtureを元に、ソースコードを管理するデータベース
///
/// ファイルシステムからソースコードを読み込むわけではありません。
/// 単純な文字列リテラルをソースコードとして扱います。
/// そのため、ファイルシステムを使用する代わりにpanicします。
pub struct FixtureDatabase {
    source_root: FileId,
    _file_by_path: HashMap<FilePath, FileId>,
    _path_by_file: HashMap<FileId, FilePath>,
    file_contents: HashMap<FileId, String>,
    file_path_interner: FilePathInterner,
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
    /// use hir::FixtureDatabase;
    /// let fixture_db = FixtureDatabase::new("
    ///    //- /main.nail
    ///    mod foo;
    ///    fn main() {
    ///      foo::bar();
    ///    }
    ///    //- /foo.nail
    ///    pub fn bar() -> i32 {
    ///      10
    ///    }
    /// ");
    pub fn new(fixture: &str) -> Self {
        let fixture = Fixture::parse(fixture);

        Self {
            source_root: fixture.source_root,
            _file_by_path: fixture.file_by_path,
            _path_by_file: fixture.path_by_file,
            file_contents: fixture.file_contents,
            file_path_interner: fixture.file_path_interner,
        }
    }
}
impl SourceDatabaseTrait for FixtureDatabase {
    fn source_root(&self) -> FileId {
        self.source_root
    }

    /// 保持しているファイルパスを元に、ファイルIDを取得します。
    /// 本来、ファイルパスが存在しない場合は、そのファイルを読み込み保存しますが、
    /// テスト用のため、ファイルシステムを使用する代わりにpanicします。
    ///
    /// # Panics
    ///
    /// ファイルパスが見つからない場合はpanicします。
    fn register_file_with_read(&mut self, path: &str) -> FileId {
        let Some(file_path) = self.file_path_interner.get(path) else { panic!("Not found file path: {path}") };
        FileId(file_path)
    }

    fn content(&self, file_id: FileId) -> Option<&str> {
        self.file_contents
            .get(&file_id)
            .map(|content| content.as_str())
    }
}

/// ファイルシステムを使用しない、ファイルレスのソースコードを管理するデータベース
/// ファイルシステムを使用する場合、代わりにpanicします。
/// 主なユースケースは、REPLやプレイグラウンドです。
pub struct FilelessSourceDatabase {
    dummy_source_root: FileId,
    content: String,
    _file_path_interner: FilePathInterner,
}
impl FilelessSourceDatabase {
    /// 入力元のソースコードを構成する文字列をパースして、データベースを構築します。
    ///
    /// # Arguments
    ///
    /// * `content` - ソースコードを構成する文字列
    pub fn new(content: &str) -> Self {
        let mut file_path_interner = FilePathInterner::new();
        let dummy_source_root = FileId(file_path_interner.intern(".dummy"));
        Self {
            dummy_source_root,
            content: content.to_string(),
            _file_path_interner: file_path_interner,
        }
    }
}
impl SourceDatabaseTrait for FilelessSourceDatabase {
    fn source_root(&self) -> FileId {
        self.dummy_source_root
    }

    fn register_file_with_read(&mut self, _path: &str) -> FileId {
        unreachable!("unsupported registering file");
    }

    fn content(&self, file_id: FileId) -> Option<&str> {
        if file_id != self.dummy_source_root {
            panic!("not source root file: {file_id:?}");
        }

        Some(self.content.as_str())
    }
}

/// ファイルシステムを使用する、ソースコードを管理するデータベース
pub struct SourceDatabase {
    source_root: Option<FileId>,
    file_by_path: HashMap<FilePath, FileId>,
    path_by_file: HashMap<FileId, FilePath>,
    file_contents: HashMap<FileId, String>,
    file_path_interner: FilePathInterner,
}
impl SourceDatabase {
    /// エントリポイントのソースコードをパースして、データベースを構築します。
    ///
    /// # Arguments
    ///
    /// * `root_file_path` - ソースコードのルートファイルのパス
    pub fn new(root_file_path: &str) -> Self {
        let mut db = Self {
            source_root: None,
            file_by_path: HashMap::new(),
            file_contents: HashMap::new(),
            path_by_file: HashMap::new(),
            file_path_interner: FilePathInterner::new(),
        };
        let root_file_id = db.register_file_with_read(root_file_path);
        db.source_root = Some(root_file_id);

        db
    }
}
impl SourceDatabaseTrait for SourceDatabase {
    fn source_root(&self) -> FileId {
        self.source_root.unwrap()
    }

    fn register_file_with_read(&mut self, path: &str) -> FileId {
        let contents = std::fs::read_to_string(path).unwrap();

        let path = self.file_path_interner.intern(path);
        let file_id = FileId(path);
        self.file_by_path.insert(path, file_id);
        self.path_by_file.insert(file_id, path);
        self.file_contents.insert(file_id, contents);
        file_id
    }

    fn content(&self, file_id: FileId) -> Option<&str> {
        self.file_contents.get(&file_id).map(|s| s.as_str())
    }
}
