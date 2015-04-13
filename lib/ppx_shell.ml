
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Shell
open Environment_mapper

let shell_mapper argv =
  (* Our getenv_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      (* Is this an extension node? *)
      | { pexp_desc = Pexp_extension ({ txt = "sh"; loc }, pstr) } ->
        begin match pstr with
        | (* Should have a single structure item, which is evaluation of a constant string. *)
          PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] ->
          [%expr sym]
        | _ ->
          raise (Location.Error (
              Location.error ~loc "Invalid use of [%sh], see README"))
        end
      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  }

let () = register "env" Environment_mapper.mapper
