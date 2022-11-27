#![no_main]

use libfuzzer_sys::fuzz_target;

use ast::AstNode;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let parse = parser::parse(s);
        let syntax = parse.syntax();
        let _validation_errors = ast::validation::validate(&syntax);
        let source_file = ast::SourceFile::cast(syntax).unwrap();
        let (_, ..) = hir::lower(source_file);
    }
});
