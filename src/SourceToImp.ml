module Src = SourceLocalisedAST
module Imp = ImpAST
open CommonAST

let rec strip_location l context args locals =
  match l with
  | Src.Identifier(i) -> (Imp.Identifier(i), false)
  | Src.ArrayAccess(e1, e2) ->
     let (expr1, full_convention1) = strip_expression e1 context args locals in
     let (expr2, full_convention2) = strip_expression e2 context args locals in
     (Imp.BlockAccess(expr1, expr2), full_convention1 || full_convention2)
  | Src.FieldAccess(e, s) ->
     let e_typ = SourceTypeChecker.type_expression context args e locals in
     begin [@ocaml.warning "-8"]
       match e_typ with
       | TypStruct(x) ->
	  let l = Symb_Tbl.find x context.struct_types in
	  let rec index_of_elt l acc =
	    match l with
	    | [] -> failwith (Printf.sprintf "struct %s doesn't have field %s" x s)
	    | (str, typ)::tl -> if String.equal str s then acc else index_of_elt tl (acc+1)
	  in
	  let (expr, full_convention) = strip_expression e context args locals in
	  (Imp.BlockAccess(expr, Imp.Literal(Int(index_of_elt l.fields 0))), full_convention)
     end

and strip_expression e context args locals =
  begin [@ocaml.warning "-8"]
      match Src.(e.expr) with
      | Src.Literal(l) -> (Imp.Literal(l), false)
      | Src.Location(l) ->
	 let (loc, full_convention) = strip_location l context args locals in
	 (Imp.Location(loc), full_convention)
      | Src.NewArray2(e, t) ->
	 let (expr, full_convention) = strip_expression e context args locals in
	 (Imp.NewBlock(expr), full_convention)
      | Src.NewArray(e1, e2) ->
	 let (expr, full_convention) = strip_expression e1 context args locals in
	 (Imp.NewBlock(expr), full_convention)
      | Src.NewRecord(s) -> (Imp.NewBlock(Imp.Literal(Int(Symb_Tbl.cardinal context.struct_types))), false)
      | Src.FunCall(Id(i), l) ->
	 let args_str = List.fold_left (fun acc elt -> acc^"_"^(print_typ (SourceTypeChecker.type_expression context args elt locals))) "" l in
	 (Imp.FunCall(Id(i^args_str), List.map (fun e -> let (expr, _) = strip_expression e context args locals in expr) l), true)
      | Src.UnaryOp(Not, e) ->
	 let (expr, full_convention) = strip_expression e context args locals in
	 let res = match expr with
	   | Imp.Literal(Bool(i)) -> Imp.Literal(Bool(not i))
	   | Imp.UnaryOp(Not, e) -> e
	   | _ -> Imp.UnaryOp(Not, expr)
	 in (res, full_convention)
      | Src.UnaryOp(Minus, e) ->
	 let (expr, full_convention) = strip_expression e context args locals in
	 let res = match expr with
	   | Imp.Literal(Int(i)) -> Imp.Literal(Int(-i))
	   | Imp.UnaryOp(Minus, e) -> e
	   | _ -> Imp.UnaryOp(Minus, expr)
	 in (res, full_convention)
      | Src.BinaryOp(op, e1, e2) ->
	 let (expr1, full_convention1) = strip_expression e1 context args locals in
	 let (expr2, full_convention2) = strip_expression e2 context args locals in
	 let res = match (expr1, expr2) with
	   | (Imp.Location(Imp.Identifier(Id(i1))), Imp.Location(Imp.Identifier(Id(i2)))) ->
	      let res2 =
		if String.equal i1 i2 then
		  begin
		    match op with
		    | Eq -> Imp.Literal(Bool(true))
		    | Neq -> Imp.Literal(Bool(false))
		    | Lt -> Imp.Literal(Bool(false))
		    | Le -> Imp.Literal(Bool(true))
		    | Gt -> Imp.Literal(Bool(false))
		    | Ge -> Imp.Literal(Bool(true))
		    | And -> expr1
		    | Or -> expr1
		    | _ -> Imp.BinaryOp(op, expr1, expr2)
		  end
		else Imp.BinaryOp(op, expr1, expr2)
	      in res2
	   | (Imp.Literal(Int(i1)), Imp.Literal(Int(i2))) ->
	      let res2 = match op with
		| Add -> Imp.Literal(Int(i1 + i2))
		| Sub -> Imp.Literal(Int(i1 - i2))
		| Div -> if i2 != 0 then Imp.Literal(Int(i1 / i2)) else raise (Division_by_zero (e2.e_pos))
		| Mult -> Imp.Literal(Int(i1 * i2))
		| Mod -> Imp.Literal(Int(i1 mod i2))
		| Eq -> Imp.Literal(Bool(i1 = i2))
		| Neq -> Imp.Literal(Bool(i1 != i2))
		| Lt -> Imp.Literal(Bool(i1 < i2))
		| Le -> Imp.Literal(Bool(i1 <= i2))
		| Gt -> Imp.Literal(Bool(i1 > i2))
		| Ge -> Imp.Literal(Bool(i1 >= i2))
	      in res2
	   | (Imp.Literal(Int(0)), (_ as expr)) ->
	      let res2 = match op with
		| Add -> expr
		| Sub -> expr
		| Mult | Div -> Imp.Literal(Int(0))
		| _ -> Imp.BinaryOp(op, expr1, expr2)
	      in res2
	   | ((_ as expr), Imp.Literal(Int(0))) ->
	      let res2 = match op with
		| Add -> expr
		| Sub -> expr
		| Mult -> Imp.Literal(Int(0))
		| Div -> raise (Division_by_zero (e2.e_pos))
		| _ -> Imp.BinaryOp(op, expr1, expr2)
	      in res2
	   | (Imp.Literal(Int(1)), (_ as expr)) ->
	      let res2 = match op with
		| Mult -> expr
		| _ -> Imp.BinaryOp(op, expr1, expr2)
	      in res2
	   | ((_ as expr), Imp.Literal(Int(1))) ->
	      let res2 = match op with
		| Mult | Div -> expr
		| _ -> Imp.BinaryOp(op, expr1, expr2)
	      in res2
	   | (Imp.Literal(Bool(i1)), Imp.Literal(Bool(i2))) ->
	      let res2 = match op with
		| Eq -> Imp.Literal(Bool(i1 = i2))
		| Neq -> Imp.Literal(Bool(i1 != i2))
		| And -> Imp.Literal(Bool(i1 && i2))
		| Or -> Imp.Literal(Bool(i1 || i2))
	      in res2
	   | (Imp.Literal(Bool(i)), (_ as expr)) | ((_ as expr), Imp.Literal(Bool(i))) ->
	      let res2 = match op with
		| And -> if i then expr else Imp.Literal(Bool(false))
		| Or -> if i then Imp.Literal(Bool(true)) else expr
		| _ -> Imp.BinaryOp(op, expr1, expr2)
	      in res2
	   | (Imp.Literal(Int(i1)), BinaryOp(Add, Literal(Int(i2)), (_ as expr))) | (BinaryOp(Add, Literal(Int(i2)), (_ as expr)), Imp.Literal(Int(i1)))
	   | (Imp.Literal(Int(i1)), BinaryOp(Add, (_ as expr), Literal(Int(i2)))) | (BinaryOp(Add, (_ as expr), Literal(Int(i2))), Imp.Literal(Int(i1))) ->
	      let res2 = match op with
		| Add -> if i1 + i2 != 0 then Imp.BinaryOp(Add, Imp.Literal(Int(i1 + i2)), expr) else expr
		| _ -> Imp.BinaryOp(op, expr1, expr2)
	      in res2
	   | (Imp.Literal(Int(i1)), BinaryOp(Mult, Literal(Int(i2)), (_ as expr))) | (BinaryOp(Mult, Literal(Int(i2)), (_ as expr)), Imp.Literal(Int(i1)))
	   | (Imp.Literal(Int(i1)), BinaryOp(Mult, (_ as expr), Literal(Int(i2)))) | (BinaryOp(Mult, (_ as expr), Literal(Int(i2))), Imp.Literal(Int(i1))) ->
	      let res2 = match op with
		| Mult ->
		   let res = i1 * i2 in
		   if res = 0 then Imp.Literal(Int(0))
		   else if res = 1 then expr
		   else Imp.BinaryOp(Mult, Imp.Literal(Int(i1 * i2)), expr)
		| _ -> Imp.BinaryOp(op, expr1, expr2)
	      in res2
	   | _ -> Imp.BinaryOp(op, expr1, expr2)
	 in (res, full_convention1 || full_convention2)
  end

let rec strip_instruction i context args locals = match Src.(i.instr) with
  | Src.Nop -> (Imp.Nop, false)
  | Src.Break -> (Imp.Break, false)
  | Src.Continue -> (Imp.Continue, false)
  | Src.FunCall(Id(i), l) ->
     let args_str = List.fold_left (fun acc elt -> acc^"_"^(print_typ (SourceTypeChecker.type_expression context args elt locals))) "" l in
     (Imp.FunCall(Id(i^args_str), List.map (fun e -> let (expr, conv) = strip_expression e context args locals in expr) l), true)
  | Src.Return(e) ->
     let (expr, conv) = strip_expression e context args locals in
     (Imp.Return(expr), conv)
  | Src.Set(l, e) ->
     let (loc, full_convention1) = strip_location l context args locals in
     let (expr, full_convention2) = strip_expression e context args locals in
     (Imp.Set(loc, expr), full_convention1 || full_convention2)
  | Src.Conditional(e, i1, i2) ->
     let (expr, full_convention1) = strip_expression e context args locals in
     let (instr1, full_convention2) = strip_instruction i1 context args locals in
     let (instr2, full_convention3) = strip_instruction i2 context args locals in
     (Imp.Conditional(expr, instr1, instr2), full_convention1 || full_convention2 || full_convention3)
  | Src.Loop(e, i, inc) ->
     let (expr, full_convention1) = strip_expression e context args locals in
     let (instr1, full_convention2) = strip_instruction i context args locals in
     let (instr2, full_convention3) = strip_instruction inc context args locals in
     (Imp.Loop(expr, instr1, instr2), full_convention1 || full_convention2 || full_convention3)
  | Src.Sequence(i1, i2) ->
     let (instr1, full_convention1) = strip_instruction i1 context args locals in
     let (instr2, full_convention2) = strip_instruction i2 context args locals in
     (Imp.Sequence(instr1, instr2), full_convention1 || full_convention2)
      
let strip_program p =
  try
    let context = (SourceTypeChecker.extract_context p) in
    let (main, full_convention) = strip_instruction Src.(p.main) context [] ((Symb_Tbl.find "main_integer" p.functions).locals) in
    let globals = Src.(p.globals) in
    let structs = Src.(p.structs) in
    let functions = Symb_Tbl.mapi (fun k v -> let (code, full_convention) = strip_instruction Src.(v.code) context v.signature.formals v.locals in Imp.({ signature = Src.(v.signature); code = code; locals = v.locals; full_convention = full_convention })) (Src.(p.functions)) in
    Imp.({ main; globals; structs; functions; })
  with
  | Division_by_zero(l, c) -> Printf.printf "arithmetic exception: division by zero at line %d and column %d\n" l c; exit (-1)
