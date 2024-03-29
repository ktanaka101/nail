#[cfg(test)]
mod tests {
    use expect_test::expect_file;
    use regex::Regex;

    async fn check(dir_name: &str) {
        let mut out = Vec::new();
        let mut err = Vec::new();

        let main_file_contents =
            std::fs::read_to_string(format!("tests/pod_execution/{dir_name}/pod.toml")).unwrap();
        // //---stdoutで区切ることで、標準出力と標準エラー出力をテストする
        // //---stderrは必ず//---stdoutより後に書くことが前提
        let (main_contents, _) = main_file_contents
            .split_once("//---stdout")
            .unwrap_or((&main_file_contents, ""));
        let mut main_contents = main_contents.trim().to_string();

        match dock::execute(
            dock::NailExecutablePath::PodFile(
                std::env::current_dir()
                    .unwrap()
                    .join(format!("tests/pod_execution/{dir_name}/pod.toml")),
            ),
            &mut out,
            &mut err,
            false,
        )
        .await
        {
            Ok(_) => (),
            Err(e) => match e {
                // 型チェックのエラーはExecutionError::Nailで返るため何もしない
                // それ以外は型チェック以外のエラーなのでpanicしてテストを失敗させる
                dock::ExecutionError::Nail => (),
                dock::ExecutionError::Io(e) => panic!("io error: {e}"),
                dock::ExecutionError::Join(e) => panic!("join error: {e}"),
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

        expect_file![format!("pod_execution/{dir_name}/pod.toml")].assert_eq(&main_contents);
    }

    #[tokio::test]
    async fn mod_project_with_pod_file() {
        check("mod_project_with_pod_file").await;
    }
}
