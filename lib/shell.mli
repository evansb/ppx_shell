
module type Command_sig = sig
  type t
  val empty : t
  (** creates empty command that does nothing when executed *)

  val from_parsetree : Parsetree.expression -> t
  (** creates command from an OCaml expression *)

  val evaluate : Env.t -> t -> (int * string)
  (** evaluates a command, returning the exit code and the stdout *)
end

module type Env_sig = sig
  type t
  val empty : unit -> t
  (** creates empty environment *)

  val from_parsetree : Parsetree.expression -> t
  (** creates an environment from OCaml expression *)

  val from_assoc_list : (string * string) list -> t
  (** creates an environment from association list *)
end

module Command : Command_sig
module Env : Env_sig
