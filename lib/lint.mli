type check

val dune_project : string -> check
(** [dune_project s] checks whether the [dune-project] file at [s] is
    valid and returns with an error if not.

    Validity is determined by:
    - Does the version include the [+dune] suffix?
    - Is the [name] specified?
    *)

val eval : check -> (unit, [ `Msg of string ]) Result.t list

val opam_uses_dune : string -> check
(** [opam_uses_dune s] checks whether the OPAM file at [s] uses the dune binary
    in the build instructions of the OPAM file. It doesn't check whether dune
    actually will build the artifacts but is just an heuristic. *)

val all : label:string -> check list -> check

(**/**)

(* Undocumented: internal and only exposed for tests *)

module Private : sig
  val check_version :
    string -> [ `Finished of (unit, [ `Msg of string ]) result ]
end

(**/**)
