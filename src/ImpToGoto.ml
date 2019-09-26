module Imp = ImpAST
module Gto = GotoAST
open CommonAST

let (++) = Gto.(++)
  
let new_label =
  let cpt = ref 0 in
  fun () -> incr cpt; CommonAST.Lab (Printf.sprintf "_label_%i" !cpt)

let rec translate_location = function
  | Imp.Identifier(i) -> Gto.Identifier(i)
  | Imp.BlockAccess(e1, e2) -> Gto.BlockAccess(translate_expression e1, translate_expression e2)
    
and translate_expression = function
  | Imp.Literal(l) -> Gto.Literal(l)
  | Imp.Location(l) -> Gto.Location(translate_location l)
  | Imp.NewBlock(e) -> Gto.NewBlock(translate_expression e)
  | Imp.FunCall(i, l) -> Gto.FunCall(i, List.map (fun e -> translate_expression e) l)
  | Imp.UnaryOp(op, e) -> Gto.UnaryOp(op, translate_expression e)
  | Imp.BinaryOp(op, e1, e2) -> Gto.BinaryOp(op, translate_expression e1, translate_expression e2)

let rec translate_instruction loop afterloop = function
  | Imp.Nop -> (Gto.Nop, 1)
  | Imp.Break ->
     let res = match afterloop with
       | None -> failwith "break statement expected within a loop" (* this case shouldn't happen we handled it in the grammar *)
       | Some(x) -> (Gto.Goto(x), 1)
     in res
  | Imp.Continue ->
     let res = match loop with
       | None -> failwith "continue statement expected within a loop" (* this case shouldn't happen we handled it in the grammar *)
       | Some(x) -> (Gto.Goto(x), 1)
     in res
  | Imp.FunCall(i, l) -> (Gto.FunCall(i, List.map (fun e -> translate_expression e) l), 1)
  | Imp.Return(e) -> (Gto.Return(translate_expression e), 1)
  | Imp.Set(l, e) -> (Gto.Set(translate_location l, translate_expression e), 1)
  | Imp.Conditional(e, i1, i2) ->
     let res = match e with
       | Imp.Literal(Bool(true)) ->
	  let (b2, t2) = (translate_instruction loop afterloop i1) in
	  (b2, t2)
       | Imp.Literal(Bool(false)) ->
	  let (b1, t1) = (translate_instruction loop afterloop i2) in
	  (b1, t1)
       | _ ->
	  let label1 = new_label () in
	  let label2 = new_label () in
	  let (b1, t1) = (translate_instruction loop afterloop i2) in
	  let (b2, t2) = (translate_instruction loop afterloop i1) in
	  if b1 = Nop && b2 = Nop then (Gto.Nop, 1) (* empty if block and else block *)
	  else if b2 = Nop then ((Gto.ConditionalGoto(label1, translate_expression e)) ++ b1 ++ Gto.Label(label1), 2 + t1) (* empty if block *)
	  else ((Gto.ConditionalGoto(label1, translate_expression e)) ++ b1 ++ (Gto.Goto(label2)) ++ Gto.Label(label1) ++ b2 ++ (Gto.Label(label2)), 4 + t1 + t2)
     in res
  | Imp.Loop(e, i, inc) ->
     let label1 = new_label () in
     let label2 = new_label () in
     let label3 = new_label () in
     let (b1, t1) = (translate_instruction (Some(label1)) (Some(label3)) i) in
     let res = match e with
       | Literal(Bool(true)) -> (Gto.Goto(label2)) ++ (Gto.Label(label3))
       | Literal(Bool(false)) -> (Gto.Label(label3))
       | _ -> (Gto.ConditionalGoto(label2, translate_expression e)) ++ (Gto.Label(label3))
     in
     if inc = Nop then
       ((Gto.Goto(label1)) ++ (Gto.Label(label2)) ++ b1 ++ (Gto.Label(label1)) ++ res, 5 + t1)
     else
       begin
	 let label4 = new_label () in
	 let (b2, t2) = (translate_instruction (Some(label1)) (Some(label3)) inc) in
	 ((Gto.Goto(label4)) ++ (Gto.Label(label2)) ++ b1 ++ (Gto.Label(label1)) ++ b2 ++ (Gto.Label(label4)) ++ res, 6 + t1 + t2)
       end
  | Imp.Sequence(i1, i2) ->
     let (b1, t1) = translate_instruction loop afterloop i1 in
     let (b2, t2) = translate_instruction loop afterloop i2 in
     (Gto.Sequence(b1, b2), t1 + t2)
     
let translate_program p =
  let (i, t) = (translate_instruction None None (Imp.(p.main))) in
  let f k v =
    let (i, t) = (translate_instruction None None (Imp.(v.code))) in
    Gto.({ signature = Imp.(v.signature); code = i; locals = v.locals; full_convention = v.full_convention; })
  in
  let functions = Symb_Tbl.mapi f (Imp.(p.functions)) in
  Gto.({
    main = i;
    globals = Imp.(p.globals);
    structs = Imp.(p.structs);
    functions = functions;
    size = t;
  })
