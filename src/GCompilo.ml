open Format

let usage = "usage: ./compilo [options] file.cid"

let file =
    let file = ref None in
    let set_file s =
      if not (Filename.check_suffix s ".cid") then
        raise (Arg.Bad "no .cid extension");
      file := Some s
    in
    Arg.parse [] set_file usage;
    match !file with Some f -> f | None -> Arg.usage [] usage; exit 1

let () =
  let c  = open_in file in
  let nc = in_channel_length c in
  let text = really_input_string c nc in
  close_in c;
  let prog = MlSourceParser.parse_text text in
  let _ = SourceTypeChecker.typecheck_program prog in
  let prog = SourceToImp.strip_program prog in
  let prog = ImpToGoto.translate_program prog in
  let output_file = (Filename.chop_suffix file ".cid") ^ ".gto" in
  (*let interpretation_file = (Filename.chop_suffix file ".cid") ^ ".out" in*)
  let out = open_out output_file in
  (*let interpretation_chan = open_out interpretation_file in*)
  (* let outf = formatter_of_out_channel out in *)
  GotoPrinter.print_program out prog;
  let _ = GotoInterpreter.eval_program prog 36 stdout in
  (* pp_print_flush outf (); *)
  close_out out;
  (*close_out interpretation_chan;*)
  exit 0
