open CommonAST
module Liveness = IndexedGotoLiveness
module Indexed = IndexedGotoAST

let rec border_effects_of_expr (e : Indexed.expression) (fun_info : Indexed.function_info) fun_border_effects_tbl =
  match e with
  | Location(BlockAccess(expr1, expr2)) ->
     (border_effects_of_expr expr1 fun_info fun_border_effects_tbl) || (border_effects_of_expr expr2 fun_info fun_border_effects_tbl)
  | UnaryOp(op, expr) -> border_effects_of_expr expr fun_info fun_border_effects_tbl
  | BinaryOp(op, expr1, expr2) ->
     (border_effects_of_expr expr1 fun_info fun_border_effects_tbl) || (border_effects_of_expr expr2 fun_info fun_border_effects_tbl)
  | NewBlock(expr) -> border_effects_of_expr expr fun_info fun_border_effects_tbl
  | FunCall(Id(id), l) ->
     if String.equal id "print_integer" || String.equal id "printInt_integer" then true
     else
       begin
	 try
	   Symb_Tbl.find id fun_border_effects_tbl || (List.fold_left (fun acc elt -> if border_effects_of_expr elt fun_info fun_border_effects_tbl then true else acc) false l)
	 with
	 | _ -> failwith ("no function found")
       end
  | _ -> false
  
let rec border_effects (i : Indexed.instruction) (fun_info : Indexed.function_info) fun_border_effects_tbl =
  match i with
  | (_, Sequence(instr1, instr2)) -> (border_effects instr1 fun_info fun_border_effects_tbl) || (border_effects instr2 fun_info fun_border_effects_tbl)
  | (_, FunCall(Id(id), l)) -> border_effects_of_expr (FunCall(Id(id), l)) fun_info fun_border_effects_tbl
  | (_, Set(Identifier(Id(id)), expr)) ->
     let res = try
		 let _ = Symb_Tbl.find id fun_info.locals in
		 false
       with
       | _ ->
	  let signature = fun_info.signature in
	  let rec aux l =
	    match l with
	    | [] -> false
	    | (s, t)::tl -> if String.equal s id && t != TypVoid && t != TypInt && t != TypBool then true else aux tl
	  in if aux signature.formals then false else true
     in res || (border_effects_of_expr expr fun_info fun_border_effects_tbl)
  | (_, Set(BlockAccess(expr1, expr2), expr)) -> (border_effects_of_expr expr1 fun_info fun_border_effects_tbl) || (border_effects_of_expr expr2 fun_info fun_border_effects_tbl) || (border_effects_of_expr expr fun_info fun_border_effects_tbl)
  | (_, Label(l)) -> false
  | (_, Goto(l)) -> false
  | (_, ConditionalGoto(l, expr)) -> border_effects_of_expr expr fun_info fun_border_effects_tbl
  | (_, Nop) -> false
  | (_, Return(expr)) -> border_effects_of_expr expr fun_info fun_border_effects_tbl

let step (i : Indexed.instruction) fun_border_effects_tbl fun_info =
  let liveness = Liveness.liveness i in
  let rec aux (i : Indexed.instruction) =
    match i with
    | (num, Set(Identifier(Id(id)), expr)) as instr ->
       if border_effects instr fun_info fun_border_effects_tbl then (instr, false) else
	 begin
	   let res = try
		       let _ = List.find (fun elt -> String.equal elt id) (liveness.live_out.(num)) in
		       (instr, false)
	     with
	     | Not_found -> ((num, Nop), true)
	   in res
	 end
    | (num, Sequence(instr1, instr2)) ->
       let (i1, b1) = aux instr1 in
       let (i2, b2) = aux instr2 in
       ((num, Sequence(i1, i2)), b1 || b2)
    | instr -> (instr, false)
  in aux i
     
let dead_code_elim (i : Indexed.instruction) fun_border_effects_tbl fun_info =
  let res = ref (step i fun_border_effects_tbl fun_info) in
  while snd (!res) do
    res := step (fst (!res)) fun_border_effects_tbl fun_info
  done;
  fst (!res)

let dead_code_elim_of_prog (p : Indexed.program) =
  let tbl = ref (Symb_Tbl.map (fun (elt : Indexed.function_info) -> false) p.functions) in
  let continue = ref true in
  let f k (fun_info : Indexed.function_info) =
    let old_res = Symb_Tbl.find k (!tbl) in
    if not old_res then
      begin
	let res = border_effects fun_info.code fun_info (!tbl) in
	if res != old_res then begin continue := true; res end else res
      end
    else old_res
  in
  while !continue do
    continue := false;
    tbl := (Symb_Tbl.mapi f p.functions)
  done;
  { p with functions = Symb_Tbl.map (fun (elt : Indexed.function_info) -> { elt with code = dead_code_elim elt.code !tbl elt }) p.functions }
