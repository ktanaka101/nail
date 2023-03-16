mod file_path_interner;

use std::collections::HashMap;

use self::file_path_interner::{FilePath, FilePathInterner};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(FilePath);

pub trait SourceDatabaseTrait {
    fn source_root(&self) -> FileId;
    fn register_file_with_read(&mut self, path: &str) -> FileId;
    fn content(&self, file_id: FileId) -> Option<&str>;
}

struct Fixture {
    source_root: FileId,
    file_by_path: HashMap<FilePath, FileId>,
    path_by_file: HashMap<FileId, FilePath>,
    file_contents: HashMap<FileId, String>,
    file_path_interner: FilePathInterner,
}
impl Fixture {
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
            if file_path == "/main.rs" {
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
                        if file_path == "/main.rs" {
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

        let Some(source_root) = source_root else { panic!("source root is not found. need: `/main.rs`") };

        Self {
            source_root,
            file_by_path,
            path_by_file,
            file_contents,
            file_path_interner,
        }
    }
}

pub struct FixtureDatabase {
    source_root: FileId,
    _file_by_path: HashMap<FilePath, FileId>,
    _path_by_file: HashMap<FileId, FilePath>,
    file_contents: HashMap<FileId, String>,
    file_path_interner: FilePathInterner,
}
impl FixtureDatabase {
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

pub struct FilelessSourceDatabase {
    dummy_source_root: FileId,
    content: String,
    _file_path_interner: FilePathInterner,
}
impl FilelessSourceDatabase {
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

pub struct SourceDatabase {
    source_root: Option<FileId>,
    file_by_path: HashMap<FilePath, FileId>,
    path_by_file: HashMap<FileId, FilePath>,
    file_contents: HashMap<FileId, String>,
    file_path_interner: FilePathInterner,
}
impl SourceDatabase {
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
