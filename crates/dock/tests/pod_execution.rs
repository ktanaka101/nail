//! Test the pod execution.
mod common;

#[cfg(test)]
mod tests {
    fn check(dir_name: &str) {
        crate::common::check_pod(&format!("pod_execution/{dir_name}"));
    }

    #[test]
    fn mod_project_with_pod_file() {
        check("mod_project_with_pod_file");
    }
}
