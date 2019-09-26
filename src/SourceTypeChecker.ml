open CommonAST
open SourceLocalisedAST

exception Type_error of typ * typ * (int * int)

(*let rec print_typ t =
  match t with
  | TypVoid -> Printf.printf "void"
  | TypInt -> Printf.printf "integer"
  | TypBool -> Printf.printf "boolean"
  | TypStruct(x) -> Printf.printf "%s" x
  | TypArray(x) -> print_typ (x); Printf.printf " array"*)
    
let rec check_type context args e expected_type locals =
  let e_type = type_expression context args e locals in
  let rec aux t exp =
    match (exp, t) with
    | (TypInt, TypInt) | (TypBool, TypBool) | (TypVoid, TypVoid) -> ()
    | (TypArray(x), TypArray(y)) -> aux y x
    | (TypStruct(x), TypStruct(y)) -> if String.equal x y then () else raise (Type_error(e_type, expected_type, e.e_pos))
    | _ -> raise (Type_error(e_type, expected_type, e.e_pos))
  in
  match (expected_type, e_type) with
  | (TypInt, TypInt) | (TypBool, TypBool) | (TypVoid, TypVoid) -> ()
  | (TypArray(x), TypArray(y)) -> aux y x
  | (TypStruct(x), TypStruct(y)) -> if String.equal x y then () else raise (Type_error(e_type, expected_type, e.e_pos))
  | _ -> raise (Type_error(e_type, expected_type, e.e_pos))

and type_expression context args e locals = match e.expr with
  | Literal lit -> type_literal lit
  | Location loc -> type_location context args locals loc
  | UnaryOp(Minus, e) -> check_type context args e TypInt locals; TypInt
  | UnaryOp(Not, e) -> check_type context args e TypBool locals; TypBool
  | BinaryOp(op, e1, e2) ->
     let operand_type, result_type = match op with
       | Add | Sub | Mult | Div | Mod -> TypInt, TypInt
       | Lt | Le | Gt | Ge -> TypInt, TypBool
       | And | Or -> TypBool, TypBool
       | Eq | Neq -> type_expression context args e1 locals, TypBool
     in
     check_type context args e1 operand_type locals;
     check_type context args e2 operand_type locals;
     result_type
  | NewArray(e1, e2) ->
     check_type context args e1 TypInt locals;
    TypArray(type_expression context args e2 locals)
  | NewArray2(e, typ) ->
     check_type context args e TypInt locals;
    TypArray(typ)
  | NewRecord(s) -> TypStruct(s)
  | FunCall(Id(i), l) ->
     let args_str = List.fold_left (fun acc elt -> acc^"_"^(print_typ (type_expression context args elt locals))) "" l in
     let signature = try
		       Symb_Tbl.find (i^args_str) context.function_signatures
     with
     | Not_found ->
	let (li, col) = e.e_pos in
	let str = (List.fold_left (fun acc elt -> acc^", "^(print_typ (type_expression context args elt locals))) "" l) in
	failwith (Printf.sprintf "function %s(%s) not previously declared at line %d and column %d" i (String.sub str 2 ((String.length str) - 2)) li col)
     in
     signature.return

and type_literal = function
  | Int _ -> TypInt
  | Bool _ -> TypBool

and type_location context args locals = function
  | Identifier(Id id) ->
     let res = try
		 Symb_Tbl.find id locals
       with
       | Not_found ->
	  let res' = try
		       snd (List.find (fun elt -> String.equal (fst elt) id) args)
	    with
	    | Not_found ->
	       let res'' = try
			     Symb_Tbl.find id context.identifier_types
		 with
		 | Not_found -> failwith (Printf.sprintf "var %s not previously declared" id)
	       in res''
	  in res'
     in res
  | ArrayAccess(e1, e2) ->
     check_type context args e2 TypInt locals;
    let res = match type_expression context args e1 locals with
      | TypArray(x) -> x
      | x -> raise (Type_error(x, TypArray(x), e1.e_pos))
    in res
  | FieldAccess(e, s) ->
     let e_typ = type_expression context args e locals in
     match e_typ with
     | TypStruct(x) ->
	let rec field_typ l =
	  match l with
	  | [] -> failwith (Printf.sprintf "field %s of struct %s doesn't exist" s x)
	  | (f, t)::tl -> if String.equal s f then t else field_typ tl
	in
	let arg = Symb_Tbl.find x context.struct_types in field_typ arg.fields
     | x -> raise (Type_error(x, TypStruct("struct"), e.e_pos))

let rec typecheck_instruction context args i fun_typ locals = match i.instr with
  | FunCall(Id(id), l) -> check_type context args ({ expr = FunCall(Id(id), l); e_pos = i.i_pos }) TypVoid locals
  | Return(e) ->
     let t = type_expression context args e locals in
     let () = match fun_typ with
       | TypVoid -> raise (Type_error(t, TypVoid, i.i_pos))
       | _ -> check_type context args e fun_typ locals
     in ()
  | Set(loc, e) ->
     let loc_type = type_location context args locals loc in
     check_type context args e loc_type locals
  | Conditional(e, i1, i2) ->
     check_type context args e TypBool locals;
    typecheck_instruction context args i1 fun_typ locals;
    typecheck_instruction context args i2 fun_typ locals
  | Loop(e, i, inc) ->
     check_type context args e TypBool locals;
    typecheck_instruction context args i fun_typ locals;
    typecheck_instruction context args inc fun_typ locals
  | Sequence(i1, i2) ->
     typecheck_instruction context args i1 fun_typ locals;
     typecheck_instruction context args i2 fun_typ locals
  | Break -> ()
  | Continue -> ()
  | Nop -> ()

let extract_context p =
  let t1 = Symb_Tbl.singleton "printInt_integer" ({ return=TypInt; formals=["x", TypInt] }) in
  let t2 = Symb_Tbl.add "power_integer_integer" ({ return=TypInt; formals=["x", TypInt; "n", TypInt] }) t1 in
  let t3 = Symb_Tbl.add "print_integer" ({ return=TypVoid; formals=["x", TypInt] }) t2 in
  { identifier_types = p.globals; struct_types = p.structs; function_signatures = Symb_Tbl.union (fun v1 v2 -> failwith(Printf.sprintf "function %s previously declared" v1)) (Symb_Tbl.map (fun v -> v.signature) p.functions) t3 }

(*let rec print_typ t =
  match t with
  | TypVoid -> "void"
  | TypInt -> "integer"
  | TypBool -> "boolean"
  | TypStruct(x) -> Printf.sprintf "%s" x
  | TypArray(x) -> Printf.sprintf "%s array" (print_typ x)*)
    
let typecheck_program p =
  let type_context = extract_context p in
  let var_list = Symb_Tbl.bindings p.globals in
  let rec check_structs l =
    match l with
    | [] -> ()
    | (k, v)::s ->
       match v with
       | TypStruct(x) ->
	  let () = try
		     let _ = Symb_Tbl.find x p.structs in check_structs s
	    with
	    | Not_found -> failwith (Printf.sprintf "type %s doesn't exist" x)
	  in check_structs s
       | _ -> check_structs s
  in
  let () = check_structs var_list in
  let rec check_functions l =
    match l with
    | [] -> ()
    | (k, v)::s ->
       let args = v.signature.formals in
       let () = typecheck_instruction type_context args v.code v.signature.return v.locals in
       check_functions s
  in
	  
  let () = try
	     let () = check_functions (Symb_Tbl.bindings p.functions) in
	     let main_infos = Symb_Tbl.find "main_integer" p.functions in
	     typecheck_instruction type_context main_infos.signature.formals p.main main_infos.signature.return main_infos.locals
    with
    | Type_error(t1, t2, (l, c)) ->
       let s1 = print_typ t1 in
       let s2 = print_typ t2 in
       Printf.printf "error at line %d and column %d: found type [%s] but type [%s] was expected\n" l c s1 s2; exit (-1)
  in ()
