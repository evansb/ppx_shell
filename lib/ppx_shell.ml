
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Shell
open Environment_mapper

let error_at loc = raise (Location.Error (Location.error ~loc
  "Invalid use of [%sh], see README"))

let script_to_id_list s =
  let pat = "\\$(\\w+)\\s?" in
  try
    let matches = Pcre.exec_all ~pat s in
    Array.to_list (Array.map (fun m -> (Pcre.get_substrings m).(1)) matches)
  with _ -> []

let mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "env"; loc }, _) } ->
          (Environment_mapper.mapper argv).expr mapper expr
      | { pexp_desc = Pexp_extension ({ txt = "sh"; loc }, expr) } ->
        begin match expr with
          | PStr [{ pstr_desc = Pstr_eval ({ pexp_loc = loc; pexp_desc = e}, _)}] ->
            begin match e with
              | Pexp_constant ((Const_string (ss, _)) as s) ->
                let id_list = script_to_id_list ss in
                let assoc_list =
                  Environment_mapper.list_to_expr_assoc_list id_list in
                let env = [%expr (Environment.from_assoc_list [%e assoc_list])] in
                [%expr (Shell.evaluate ~env:[%e env] [%e (Exp.constant ~loc s)])]
              | Pexp_apply (e, es) ->
                let env = (Environment_mapper.mapper argv).expr mapper e in
                begin match es with
                  | [(_, script)] ->
                    begin match script.pexp_desc with
                      | Pexp_constant ((Const_string _) as s) ->
                        [%expr (Shell.evaluate ~env:[%e env]
                          [%e (Exp.constant ~loc:script.pexp_loc s)])]
                      | _ -> error_at script.pexp_loc
                    end
                  | _ -> error_at e.pexp_loc
                end
              | _ -> error_at loc
            end
          | _ -> error_at loc
        end
      | x -> default_mapper.expr mapper x;
  }

let () = register "sh" mapper;
