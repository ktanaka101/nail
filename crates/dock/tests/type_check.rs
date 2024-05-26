mod common;

#[cfg(test)]
mod tests {
    fn check(dir_name: &str) {
        crate::common::check_single(&format!("type_check/{dir_name}"));
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

    #[test]
    fn init_not_record() {
        check("init_not_record");
    }

    #[test]
    fn init_record() {
        check("init_record");
    }

    #[test]
    fn init_struct_tuple_mismatch() {
        check("init_struct_tuple_mismatch");
    }

    #[test]
    fn missing_struct_record_field() {
        check("missing_struct_record_field");
    }

    #[test]
    fn no_such_struct_record_field() {
        check("no_such_struct_record_field");
    }

    #[test]
    fn init_struct_record_mismatch() {
        check("init_struct_record_mismatch");
    }
}
