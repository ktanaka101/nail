#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use expect_test::expect_file;

    #[tokio::test]
    async fn return_type() {
        let mut out = Vec::new();
        let mut err = Vec::new();

        dock::execute(
            std::env::current_dir()
                .unwrap()
                .join(PathBuf::from("tests/return_type/main.nail"))
                .to_str()
                .unwrap(),
            &mut out,
            &mut err,
        )
        .await
        .unwrap_err();

        expect_file!["return_type/main.nail.stdout"].assert_eq(&String::from_utf8(out).unwrap());
        expect_file!["return_type/main.nail.stderr"].assert_eq(&String::from_utf8(err).unwrap());
    }
}
