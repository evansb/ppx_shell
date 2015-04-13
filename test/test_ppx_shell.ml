
open OUnit

open Shell

let env_to_string _ =
  let e = Environment.from_assoc_list [("foo", "2"); ("bar", "3")] in
  let s = Environment.to_string e in
  assert_equal (s = "foo=2\nbar=3\n" || s = "bar=3\nfoo=2\n") true

let backend_execute_env _ =
  let e = Environment.from_assoc_list [("foo", "2")] in
  let c = Command.from_string "echo $foo" in
  let (exit_code, output) = Shell.evaluate e c in
  assert_equal exit_code 0;
  assert_equal output "2\n"

let backend_execute_noenv _ =
  let e = Environment.empty () in
  let c = Command.from_string "echo Hello World" in
  let (exit_code, output) = Shell.evaluate e c in
  assert_equal exit_code 0;
  assert_equal output "Hello World\n"

let environment_extension _ =
  let a = "1" in
  assert_equal [%env a] (Environment.singleton ("a", "1"))

let environment_extension _ =
  let a = "1" in
  let b = "2" in
  assert_equal [%env a b] (Environment.from_assoc_list [("a", "1"); ("b", "2")])

let suite = "ppx shell test suite" >::: [
  "env_to_string" >:: env_to_string;
  "backend_execute_env" >:: backend_execute_env;
  "backend_execute_env" >:: backend_execute_noenv;
  "environment_extension_multi" >:: environment_extension
]

let _ = run_test_tt_main suite
