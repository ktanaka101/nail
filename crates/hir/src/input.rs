mod file_path_interner;

use std::collections::HashMap;

use self::file_path_interner::{FilePath, FilePathInterner};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(FilePath);

pub struct SourceRoot {
    pub files: Vec<FileId>,
}

pub struct SourceDatabase {
    source_root: SourceRoot,
    file_by_path: HashMap<FilePath, FileId>,
    path_by_file: HashMap<FileId, FilePath>,
    file_contents: HashMap<FileId, String>,
    file_path_interner: FilePathInterner,
}
impl SourceDatabase {
    fn new() -> Self {
        Self {
            source_root: SourceRoot { files: vec![] },
            file_by_path: HashMap::new(),
            file_contents: HashMap::new(),
            path_by_file: HashMap::new(),
            file_path_interner: FilePathInterner::new(),
        }
    }

    fn register_file(&self, path: &str, contents: &str) -> FileId {
        let path = self.file_path_interner.intern(path);
        let file_id = FileId(path);
        self.file_by_path.insert(path, file_id);
        self.path_by_file.insert(file_id, path);
        self.file_contents.insert(file_id, contents.to_string());
        self.source_root.files.push(file_id);
        file_id
    }
}
