val check_dune_project : string -> (unit, [ `Msg of string ]) result
val opam_uses_dune : string -> (unit, [ `Msg of string ]) result
