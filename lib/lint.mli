val check_dune_project : string -> (unit, [ `Msg of string ]) result
(** [check_dune_project s] checks whether the [dune-project] file at [s] is
    valid and returns with an error if not.

    Validity is determined by:
    - Does the version include the [+dune] suffix?
    - Is the [name] specified?
    *)

val opam_uses_dune : string -> (unit, [ `Msg of string ]) result
(** [opam_uses_dune s] checks whether the OPAM file at [s] uses the dune binary
    in the build instructions of the OPAM file. It doesn't check whether dune
    actually will build the artifacts but is just an heuristic. *)

(**/**)

(* Undocumented: internal and only exposed for tests *)

module Private : sig
  val check_version : string -> (unit, [ `Msg of string ]) result
end

(**/**)
