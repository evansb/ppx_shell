
module type Command_sig = sig
  type t = string
  (** type of the command *)

  val empty : t
  (** creates empty command that does nothing when executed *)

  val from_string : string -> t
  (** creates a command from a string *)
end

module type Environment_sig = sig
  type t = (string, string) Hashtbl.t
  (** type of the environment *)

  val empty : unit -> t
  (** creates empty environment *)

  val singleton : (string * string) -> t
  (** creates environment with one value *)

  val from_assoc_list : (string * string) list -> t
  (** creates an environment from association list *)

  val to_string : t -> string
  (** creates a key, value pair string from the environment *)
end

module Command : Command_sig
module Environment : Environment_sig

val evaluate : ?env:Environment.t -> Command.t -> int * string
