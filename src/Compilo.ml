open Format

let usage = "usage: ./compilo [options] file.cid"
let preprocessor = ref false
let indent = ref false

let file =
    let file = ref None in
    let set_file s =
      if not (Filename.check_suffix s ".cid") then
        raise (Arg.Bad "no .cid extension");
      file := Some s
    in
    Arg.parse [("-pp", Arg.Set preprocessor, "call preprocessor before compilation process");("-indent", Arg.Set indent, "use a python like syntax")] set_file usage;
    match !file with Some f -> f | None -> Arg.usage [] usage; exit 1

let () =
  let pp_file = ((Filename.chop_suffix file ".cid") ^ ".pp" ^ ".cid") in
  let () = if !preprocessor then
      begin
	let pp = open_out pp_file in
	let src_file = open_in file in
	let pp_lb = Lexing.from_channel src_file in
	Printf.fprintf pp "%s" (Preprocessor.define_or_replace pp_lb);
	close_out pp;
	close_in src_file
      end in
  let c  = open_in (if !preprocessor then pp_file else file) in
  let lb = Lexing.from_channel c in
  let prog = SourceParser.prog (if !indent then SourceLexer2.token else SourceLexer.token) lb in
  close_in c;
  let _ = SourceTypeChecker.typecheck_program prog in
  let prog = SourceToImp.strip_program prog in
  let prog_before = ImpToGoto.translate_program prog in
  let prog_indexed = IndexedGotoAST.index_program prog_before in
  let prog_with_optim = IndexedGotoDeadCodeElim.dead_code_elim_of_prog prog_indexed in
  let prog_after = IndexedGotoAST.strip_program prog_with_optim in
  let asm = GotoToMips.translate_program prog_after in
  let output_file1 = (Filename.chop_suffix file ".cid") ^ "_before_optim.gto" in
  let output_file2 = (Filename.chop_suffix file ".cid") ^ ".asm" in
  let output_file3 = (Filename.chop_suffix file ".cid") ^ "_after_optim.gto" in
  let out1 = open_out output_file1 in
  let out2 = open_out output_file2 in
  let out3 = open_out output_file3 in
  let outf = formatter_of_out_channel out2 in
  GotoPrinter.print_program out1 prog_before;
  GotoPrinter.print_program out3 prog_after;
  (*let _ = GotoInterpreter.eval_program prog 36 stdout in (* goto interpreter with arg = 36 *)*)
  Mips.print_program outf asm;
  pp_print_flush outf ();
  close_out out1;
  close_out out2;
  exit 0
