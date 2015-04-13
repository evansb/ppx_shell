
module type Command_sig = sig
  type t
  type e
  val empty : t
  val from_parsetree : Parsetree.expression -> t
  val evaluate : e -> t -> (int * string)
end

module type Environment_sig = sig
  type t
  val empty : unit -> t
  val from_parsetree : Parsetree.expression -> t
  val from_assoc_list : (string * string) list -> t
  val to_string : t -> string
end

module MakeCommand(E : Environment_sig) : (Command_sig with type e = E.t) =
struct
  type t = string

  type e = E.t

  let empty = ""

  let from_parsetree expr = empty

  let compose t1 t2 = t1 ^ "\n" ^ t2

  let evaluate_unix t =
    let open Unix in
    let ichan = open_process_in t in
    let buf = Buffer.create 96 in
    let () = try
      while true do Buffer.add_channel buf ichan 1 done
      with End_of_file -> () in
    let exit_status =
      match close_process_in ichan with
        | WEXITED n -> n
        | WSIGNALED n -> n
        | WSTOPPED n -> n
    in
    let output = Buffer.contents buf in
    (exit_status, output)

  let evaluate env t = evaluate_unix (compose (E.to_string env) t)
end

module MakeEnvironment(C : Command_sig) : Environment_sig =
struct
  type t = (string, string) Hashtbl.t

  let empty () = Hashtbl.create 10

  let from_assoc_list xs =
    let tbl = empty () in
    let () = List.iter (fun (e, v) -> Hashtbl.add tbl e v) xs in
    tbl

  let from_parsetree expr = empty ()

  let to_string t = Hashtbl.fold (fun e v acc -> (e^"="^v^"\n"^acc)) t ""
end

module rec Command : Command_sig = MakeCommand(Environment)
and Environment : Environment_sig = MakeEnvironment(Command)

