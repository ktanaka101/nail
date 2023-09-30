#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use expect_test::expect_file;

    async fn check(dir_name: &str) {
        let mut out = Vec::new();
        let mut err = Vec::new();

        let main_file_contents =
            std::fs::read_to_string(format!("tests/type_check/{dir_name}/main.nail")).unwrap();
        // //---stdoutで区切ることで、標準出力と標準エラー出力をテストする
        // //---stderrは必ず//--stdoutより後に書くことが前提
        let mut main_contents = main_file_contents
            .split("//---stdout")
            .next()
            .unwrap()
            .to_string();

        match dock::execute(
            std::env::current_dir()
                .unwrap()
                .join(PathBuf::from(&format!(
                    "tests/type_check/{dir_name}/main.nail"
                )))
                .to_str()
                .unwrap(),
            &mut out,
            &mut err,
            false,
        )
        .await
        {
            Ok(_) => (),
            Err(e) => match e {
                dock::ExecutionError::Nail => (),
                dock::ExecutionError::Io(e) => panic!("io error: {e}"),
            },
        }

        let stdout = String::from_utf8(out).unwrap();
        let stderr = String::from_utf8(err).unwrap();

        main_contents.push_str("//---stdout\n");
        if !stdout.is_empty() {
            main_contents.push('\n');
            main_contents.push_str(&stdout);
            main_contents.push('\n');
        }
        main_contents.push_str("//---stderr\n");
        if !stderr.is_empty() {
            main_contents.push('\n');
            main_contents.push_str(&stderr);
        }

        expect_file![format!("type_check/{dir_name}/main.nail")].assert_eq(&main_contents);
    }

    #[tokio::test]
    async fn return_type_match() {
        check("return_type_match").await;
    }

    #[tokio::test]
    async fn return_type_mismatch() {
        check("return_type_mismatch").await;
    }

    #[tokio::test]
    async fn return_no_expr_treated_as_unit() {
        check("return_no_expr_treated_as_unit").await;
    }

    #[tokio::test]
    async fn condition_type_match() {
        check("condition_type_match").await;
    }

    #[tokio::test]
    async fn condition_type_mismatch() {
        check("condition_type_mismatch").await;
    }

    #[tokio::test]
    async fn then_else_branch_match() {
        check("then_else_branch_match").await;
    }

    #[tokio::test]
    async fn then_else_branch_mismatch() {
        check("then_else_branch_mismatch").await;
    }

    #[tokio::test]
    async fn then_only_branch_match() {
        check("then_only_branch_match").await;
    }

    #[tokio::test]
    async fn then_only_branch_mismatch() {
        check("then_only_branch_mismatch").await;
    }

    #[tokio::test]
    async fn call_arg_type_match() {
        check("call_arg_type_match").await;
    }

    #[tokio::test]
    async fn call_arg_type_mismatch() {
        check("call_arg_type_mismatch").await;
    }
}
