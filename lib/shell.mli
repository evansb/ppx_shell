
module type Command_sig = sig
  type t
  (** type of the command *)

  type e
  (** type of the environment *)

  val empty : t
  (** creates empty command that does nothing when executed *)

  val from_parsetree : Parsetree.expression -> t
  (** creates command from an OCaml expression *)

  val evaluate : e -> t -> (int * string)
  (** evaluates a command, returning the exit code and the stdout *)
end

module type Environment_sig = sig
  type t
  (** type of the environment *)

  val empty : unit -> t
  (** creates empty environment *)

  val from_parsetree : Parsetree.expression -> t
  (** creates an environment from OCaml expression *)

  val from_assoc_list : (string * string) list -> t
  (** creates an environment from association list *)

  val to_string : t -> string
  (** creates a key, value pair string from the environment *)
end

module Command : Command_sig
module Environment : Environment_sig
