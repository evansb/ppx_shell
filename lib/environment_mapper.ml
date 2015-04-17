
open Ast_mapper
open Ast_helper
open Asttypes
open Longident
open Parsetree

let error_at loc = raise (Location.Error (Location.error ~loc
  "Invalid use of [%env], see README"))

let error_id_at loc = raise (Location.Error (Location.error ~loc
  "Only simple identifier is supported"))

let const_string s = Exp.constant (Const_string (s, None))

let expr_tuple loc s lid =
  let id = Exp.ident (Location.mkloc lid loc) in
  Exp.tuple ~loc [s ; id]

let expr_singleton lid loc =
  match lid with
  | Lident s -> (expr_tuple loc (const_string s) lid)
  | _ -> error_at loc

let one_expr e =
  match e.pexp_desc with
    | Pexp_ident { txt = id; loc } -> expr_singleton id loc
    | _ -> error_id_at e.pexp_loc

let expr_assoc_list xs =
  match xs with
    | Pexp_apply (hd, tl) ->
      begin match hd.pexp_desc with
        | Pexp_ident { txt = id; loc } ->
            let tl' = List.fold_right (fun (_, ex) acc ->
              [%expr [%e (one_expr ex)] :: [%e acc]]) tl [%expr []] in
            [%expr [%e (one_expr hd)] :: [%e tl']]
        | _ -> error_id_at hd.pexp_loc
      end
    | _ -> failwith "Impossible"

let list_to_expr_assoc_list xs =
  List.fold_right (fun x acc ->
      let open Ast_convenience in
      let t = tuple [str x; evar x] in
      [%expr [%e t] :: [%e acc]]) xs ([%expr []])

let mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "env"; loc }, expr) } ->
         begin match expr with
           | PStr [{ pstr_desc = Pstr_eval ({ pexp_loc = loc; pexp_desc = e}, _)}] ->
              begin match e with
                | Pexp_ident { txt = id; loc } ->
                  let tuple = expr_singleton id loc in
                  [%expr Shell.Environment.singleton [%e tuple]]
                | Pexp_apply (_, _) ->
                  let assoc_list = expr_assoc_list e in
                  [%expr Shell.Environment.from_assoc_list [%e assoc_list]]
                | _ -> error_at loc
              end
           | _ -> error_at loc
        end
      | x -> default_mapper.expr mapper x;
  }

