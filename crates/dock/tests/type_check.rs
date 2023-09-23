#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use expect_test::expect_file;

    async fn check(dir_name: &str) {
        let mut out = Vec::new();
        let mut err = Vec::new();

        dock::execute(
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
        .unwrap_err();

        expect_file![format!("type_check/{dir_name}/main.nail.stdout")]
            .assert_eq(&String::from_utf8(out).unwrap());
        expect_file![format!("type_check/{dir_name}/main.nail.stderr")]
            .assert_eq(&String::from_utf8(err).unwrap());
    }

    #[tokio::test]
    async fn return_type() {
        check("return_type").await;
    }
}
