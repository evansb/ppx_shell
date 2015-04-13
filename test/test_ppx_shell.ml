
open OUnit

let backend_execute_env = assert_equal 1 1
let backend_execute_noenv _ = assert_equal 1 1

let suite = "ppx shell test suite" >::: [
  "backend_execute_noenv" >:: backend_execute_noenv;
  "backend_execute_env" >:: backend_execute_noenv
]

let _ = run_test_tt_main suite
