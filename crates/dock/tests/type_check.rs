//! Test type checking.
mod common;

#[cfg(test)]
mod tests {
    use serial_test::serial;

    fn check(dir_name: &str) {
        crate::common::check_single(&format!("type_check/{dir_name}"));
    }

    #[test]
    #[serial]
    fn ok_return_type_match() {
        check("return_type_match");
    }

    #[test]
    fn err_return_type_mismatch() {
        check("return_type_mismatch");
    }

    #[test]
    fn err_return_no_expr_treated_as_unit() {
        check("return_no_expr_treated_as_unit");
    }

    #[test]
    #[serial]
    fn ok_condition_type_match() {
        check("condition_type_match");
    }

    #[test]
    fn err_condition_type_mismatch() {
        check("condition_type_mismatch");
    }

    #[test]
    #[serial]
    fn ok_then_else_branch_match() {
        check("then_else_branch_match");
    }

    #[test]
    fn err_then_else_branch_mismatch() {
        check("then_else_branch_mismatch");
    }

    #[test]
    #[serial]
    fn ok_then_only_branch_match() {
        check("then_only_branch_match");
    }

    #[test]
    fn err_then_only_branch_mismatch() {
        check("then_only_branch_mismatch");
    }

    #[test]
    #[serial]
    fn ok_call_arg_type_match() {
        check("call_arg_type_match");
    }

    #[test]
    fn err_call_arg_type_mismatch() {
        check("call_arg_type_mismatch");
    }

    #[test]
    #[serial]
    fn ok_binary_integer_match() {
        check("binary_integer_match");
    }

    #[test]
    fn err_binary_integer_mismatch() {
        check("binary_integer_mismatch");
    }

    #[test]
    #[serial]
    fn ok_binary_compare_match() {
        check("binary_compare_match");
    }

    #[test]
    fn err_binary_compare_mismatch() {
        check("binary_compare_mismatch");
    }

    #[test]
    #[serial]
    fn ok_binary_assign_match() {
        check("binary_assign_match");
    }

    #[test]
    fn err_binary_assign_mismatch() {
        check("binary_assign_mismatch");
    }

    #[test]
    #[serial]
    fn ok_unary_match() {
        check("unary_match");
    }

    #[test]
    fn err_unary_mismatch() {
        check("unary_mismatch");
    }

    #[test]
    #[serial]
    fn ok_tail_type_match() {
        check("tail_type_match");
    }

    #[test]
    fn err_tail_type_mismatch() {
        check("tail_type_mismatch");
    }

    #[test]
    #[serial]
    fn ok_call_arg_count_match() {
        check("call_arg_count_match");
    }

    #[test]
    fn err_call_arg_count_mismatch() {
        check("call_arg_count_mismatch");
    }

    #[test]
    fn err_not_callable() {
        check("not_callable");
    }

    #[test]
    fn err_module_as_expr() {
        check("module_as_expr");
    }

    #[test]
    fn err_mod_project() {
        check("mod_project");
    }

    #[test]
    fn err_loop_break_mismatch() {
        check("loop_break_mismatch");
    }

    #[test]
    fn err_loop_mismatch() {
        check("loop_mismatch");
    }

    #[test]
    fn err_break_outside_of_loop() {
        check("break_outside_of_loop");
    }

    #[test]
    fn err_while_desugared_error() {
        check("while_desugared_error");
    }

    #[test]
    fn err_init_not_record() {
        check("init_not_record");
    }

    #[test]
    #[serial]
    fn ok_init_record() {
        check("init_record");
    }

    #[test]
    fn err_init_struct_tuple_mismatch() {
        check("init_struct_tuple_mismatch");
    }

    #[test]
    fn err_missing_struct_record_field() {
        check("missing_struct_record_field");
    }

    #[test]
    fn err_no_such_struct_record_field() {
        check("no_such_struct_record_field");
    }

    #[test]
    fn err_init_struct_record_mismatch() {
        check("init_struct_record_mismatch");
    }

    #[test]
    fn err_needed_init_tuple_or_record() {
        check("needed_init_tuple_or_record");
    }

    #[test]
    fn err_no_such_field_access() {
        check("no_such_field_access");
    }

    #[test]
    fn err_can_not_field_access() {
        check("can_not_field_access");
    }

    #[test]
    fn err_not_allowed_type() {
        check("not_allowed_type");
    }
}
