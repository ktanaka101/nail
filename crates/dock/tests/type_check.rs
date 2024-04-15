#[cfg(test)]
mod tests {
    use expect_test::expect_file;
    use regex::Regex;

    fn check(dir_name: &str) {
        let mut out = Vec::new();
        let mut err = Vec::new();

        let main_file_contents =
            std::fs::read_to_string(format!("tests/type_check/{dir_name}/main.nail")).unwrap();
        // //---stdoutで区切ることで、標準出力と標準エラー出力をテストする
        // //---stderrは必ず//---stdoutより後に書くことが前提
        let (main_contents, _) = main_file_contents
            .split_once("//---stdout")
            .unwrap_or((&main_file_contents, ""));
        let mut main_contents = main_contents.trim().to_string();

        match dock::execute(
            dock::NailExecutablePath::RootFile(
                std::env::current_dir()
                    .unwrap()
                    .join(format!("tests/type_check/{dir_name}/main.nail")),
            ),
            &mut out,
            &mut err,
            false,
        ) {
            Ok(_) => (),
            Err(e) => match e {
                // 型チェックのエラーはExecutionError::Nailで返るため何もしない
                // それ以外は型チェック以外のエラーなのでpanicしてテストを失敗させる
                dock::ExecutionError::Nail => (),
                dock::ExecutionError::Io(e) => panic!("io error: {e}"),
                dock::ExecutionError::InvalidRootNailFilePath(e) => {
                    panic!("invalid root nail file path: {e}")
                }
            },
        }

        fn normalize(stdout_or_stderr: String) -> String {
            let path_pattern = Regex::new(r"(\[.*?)/crates/").unwrap();

            return stdout_or_stderr
                .lines()
                .map(|line| normalize_path(line, &path_pattern))
                .map(|line| format!("// {}", line))
                .collect::<Vec<String>>()
                .join("\n");

            /// 環境が異なってもテストが通るようにパスを正規化します。
            fn normalize_path(line: &str, path_pattern: &Regex) -> String {
                path_pattern.replace(line, "[/{nail}/crates/").to_string()
            }
        }
        let stdout = String::from_utf8(out).unwrap();
        let stderr = String::from_utf8(err).unwrap();
        let stdout = normalize(stdout);
        let stderr = normalize(stderr);

        main_contents.push_str("\n\n");
        main_contents.push_str("//---stdout\n");
        if !stdout.is_empty() {
            main_contents.push('\n');
            main_contents.push_str(&stdout);
            main_contents.push_str("\n\n");
        }
        main_contents.push_str("//---stderr\n");
        if !stderr.is_empty() {
            main_contents.push('\n');
            main_contents.push_str(&stderr);
            main_contents.push('\n');
        }

        expect_file![format!("type_check/{dir_name}/main.nail")].assert_eq(&main_contents);
    }

    #[test]
    fn return_type_match() {
        check("return_type_match");
    }

    #[test]
    fn return_type_mismatch() {
        check("return_type_mismatch");
    }

    #[test]
    fn return_no_expr_treated_as_unit() {
        check("return_no_expr_treated_as_unit");
    }

    #[test]
    fn condition_type_match() {
        check("condition_type_match");
    }

    #[test]
    fn condition_type_mismatch() {
        check("condition_type_mismatch");
    }

    #[test]
    fn then_else_branch_match() {
        check("then_else_branch_match");
    }

    #[test]
    fn then_else_branch_mismatch() {
        check("then_else_branch_mismatch");
    }

    #[test]
    fn then_only_branch_match() {
        check("then_only_branch_match");
    }

    #[test]
    fn then_only_branch_mismatch() {
        check("then_only_branch_mismatch");
    }

    #[test]
    fn call_arg_type_match() {
        check("call_arg_type_match");
    }

    #[test]
    fn call_arg_type_mismatch() {
        check("call_arg_type_mismatch");
    }

    #[test]
    fn binary_integer_match() {
        check("binary_integer_match");
    }

    #[test]
    fn binary_integer_mismatch() {
        check("binary_integer_mismatch");
    }

    #[test]
    fn binary_compare_match() {
        check("binary_compare_match");
    }

    #[test]
    fn binary_compare_mismatch() {
        check("binary_compare_mismatch");
    }

    #[test]
    fn binary_assign_match() {
        check("binary_assign_match");
    }

    #[test]
    fn binary_assign_mismatch() {
        check("binary_assign_mismatch");
    }

    #[test]
    fn unary_match() {
        check("unary_match");
    }

    #[test]
    fn unary_mismatch() {
        check("unary_mismatch");
    }

    #[test]
    fn tail_type_match() {
        check("tail_type_match");
    }

    #[test]
    fn tail_type_mismatch() {
        check("tail_type_mismatch");
    }

    #[test]
    fn call_arg_count_match() {
        check("call_arg_count_match");
    }

    #[test]
    fn call_arg_count_mismatch() {
        check("call_arg_count_mismatch");
    }

    #[test]
    fn not_callable() {
        check("not_callable");
    }

    #[test]
    fn module_as_expr() {
        check("module_as_expr");
    }

    #[test]
    fn mod_project() {
        check("mod_project");
    }

    #[test]
    fn loop_break_mismatch() {
        check("loop_break_mismatch");
    }

    #[test]
    fn loop_mismatch() {
        check("loop_mismatch");
    }

    #[test]
    fn break_outside_of_loop() {
        check("break_outside_of_loop");
    }

    #[test]
    fn while_desugared_error() {
        check("while_desugared_error");
    }
}
