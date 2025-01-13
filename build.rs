fn main() {
    pkg_config::Config::new()
        .atleast_version("8.0")
        .probe("bdw-gc")
        .expect("Could not find bdw-gc via pkg-config");
}
