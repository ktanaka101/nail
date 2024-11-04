//! Tests for checking the mutability of variables.

mod common;

#[cfg(test)]
mod tests {
    fn check(dir_name: &str) {
        crate::common::check_single(&format!("mutability_check/{dir_name}"));
    }

    #[test]
    fn reassign_immutable() {
        check("reassign_immutable");
    }

    #[test]
    fn reassign_mutable() {
        check("reassign_mutable");
    }
}
