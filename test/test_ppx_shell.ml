
open OUnit

open Shell

let env_to_string _ =
  let env = Environment.from_assoc_list [("foo", "2"); ("bar", "3")] in
  let s = Environment.to_string env in
  assert_equal (s = "foo=2\nbar=3\n" || s = "bar=3\nfoo=2\n") true

let backend_execute_env _ =
  let env = Environment.from_assoc_list [("foo", "2")] in
  let c = Command.from_string "echo $foo" in
  let (exit_code, output) = Shell.evaluate ~env c in
  assert_equal exit_code 0;
  assert_equal output "2\n"

let backend_execute_noenv _ =
  let env = Environment.empty () in
  let c = Command.from_string "echo Hello World" in
  let (exit_code, output) = Shell.evaluate ~env c in
  assert_equal exit_code 0;
  assert_equal output "Hello World\n"

let environment_extension_single _ =
  let a = "1" in
  assert_equal [%env a] (Environment.singleton ("a", "1"))

let environment_extension_multi _ =
  let a = "1" in
  let b = "2" in
  assert_equal [%env a b] (Environment.from_assoc_list [("a", "1"); ("b", "2")])

let shell_execution_simple _ =
  assert_equal [%sh {| echo Hello World |}] (0, "Hello World\n")

let shell_execution_with_var _ =
  let x = "1" in
  let y = "2" in
  assert_equal [%sh [%env x y] {| echo $(($x + $y)) |}] (0, "3\n")

let shell_execution_with_bound_var _ =
  let x = "1" in
  let y = "2" in
  let env = [%env x y] in
  assert_equal [%sh env {| echo $(($x + $y)) |}] (0, "3\n")

let shell_execution_with_capture _ =
  let echo = "echo" in
  assert_equal [%sh {| $echo Hello World |}] (0, "Hello World\n")

let suite = "ppx shell test suite" >::: [
  "env_to_string" >:: env_to_string;
  "backend_execute_env" >:: backend_execute_env;
  "backend_execute_env" >:: backend_execute_noenv;
  "environment_extension_single" >:: environment_extension_single;
  "environment_extension_multi" >:: environment_extension_multi;
  "shell_execution_simple" >:: shell_execution_simple;
  "shell_execution_with_var" >:: shell_execution_with_var;
  "shell_execution_with_bound_var" >:: shell_execution_with_bound_var;
  "shell_execution_with_capture" >:: shell_execution_with_capture
]

let _ = run_test_tt_main suite
