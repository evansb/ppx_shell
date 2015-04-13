
let e = [%env files hooligan foo]

let _ = [%sh e {| wc -w |}]
