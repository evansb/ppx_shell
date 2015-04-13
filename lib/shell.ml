
module type Command_sig = sig
  type t
  val empty : t
  val from_parsetree : Parsetree.expression -> t
  val evaluate : Env.t -> t -> (int * string)
end

module type Env_sig = sig
  type t
  val empty : unit -> t
  val from_parsetree : Parsetree.expression -> t
  val from_assoc_list : (string * string) list -> t
end

module rec Command : Command_sig = struct
  type t = string

  let empty = ""

  let from_parsetree expr = empty

  let evaluate env t = (0, "")
end

and Env : Env_sig = struct
  type t = (string, string) Hashtbl.t

  let empty () = Hashtbl.create 10

  let from_assoc_list xs =
    let tbl = empty () in
    let () = List.iter (fun (e, v) -> Hashtbl.add tbl e v) xs in
    tbl

  let from_parsetree expr = empty ()
end
