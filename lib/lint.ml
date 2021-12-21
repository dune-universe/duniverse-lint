let check_dune_project path =
  Stdio.In_channel.with_file path ~f:(fun chan ->
      let _sexp = Sexplib.Sexp.input_sexp chan in
      true)
