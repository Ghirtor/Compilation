open GotoAST
open CommonAST

type value =
  | Int of int
  | Bool of bool
  | Array of value array option
  
module State = Map.Make(String)
type state = value State.t

module Labels = Map.Make(String)
type labels = int Labels.t

let rec eval_program p arg channel =
  begin [@ocaml.warning "-8"]
  let env = ref State.empty in
  let f key value =
    match value with
    | TypInt -> env := (State.add key (Int(0)) (!env))
    | _ -> env := (State.add key (Bool(false)) (!env))
  in
  State.iter f p.globals;
  env := (State.add "arg" (Int(arg)) (!env));
  let instructions_array = Array.init p.size (fun ind -> Nop) in
  let index = ref 0 in
  let rec fill_instructions labs = function
    | Sequence(i1, i2) ->
       let labs = fill_instructions labs i1 in
       fill_instructions labs i2
    | Label(Lab(l)) as x ->
       instructions_array.(!index) <- x;
      let labs = Labels.add l (!index) labs in
      index := !index + 1;
      labs
    | x ->
       instructions_array.(!index) <- x;
      index := !index + 1;
      labs
  in
  let labs = fill_instructions (Labels.empty) (p.main) in
  let cur_instr = ref 0 in
  while (!cur_instr) < (!index) do
    let inst = instructions_array.(!cur_instr) in
    match inst with
    | Nop -> cur_instr := !cur_instr + 1
    | Set(Identifier(Id(id)), e) ->
       env := (State.add id (eval_expression !env e) !env);
      cur_instr := !cur_instr + 1
    | Print(e) ->
       cur_instr := !cur_instr + 1;
      let Int(i) = eval_expression !env e in
      Printf.fprintf channel "%c" (char_of_int i);
    | Label(Lab(l)) -> cur_instr := !cur_instr + 1
    | Goto(Lab(l)) ->
       cur_instr := Labels.find l labs
    | ConditionalGoto(Lab(l), e) ->
       let Bool(b) = eval_expression !env e in
       if b then cur_instr := Labels.find l labs
       else cur_instr := !cur_instr + 1
    | _ -> failwith "impossible case"
  done
  end

and eval_expression env = function [@ocaml.warning "-8"]
  | Literal(Int(i)) -> Int i
  | Literal(Bool(b)) -> Bool b
  | Location(Identifier(Id(id))) -> let res = try
						State.find id env;
    with
    | Not_found -> failwith "impossible case"
				    in res
  | UnaryOp(op, e) -> let v1 = eval_expression env e in
		      let v = match (op, v1) with
			| (Minus, Int(i))  -> Int (-i)
			| (Not, Bool(i))   -> Bool (not i)
		      in
                      v
  | BinaryOp(op, e1, e2) -> let v1 = eval_expression env e1 in
			    let v2 = eval_expression env e2 in
			    let res = match (v1, v2) with
			      | (Bool(i1), Bool(i2)) ->
				 let v = match op with
				   | Eq   -> Bool (i1 = i2)
				   | Neq  -> Bool (i1 != i2)
				   | And  -> Bool (i1 && i2)
				   | Or   -> Bool (i1 || i2)
				 in v
			      | (Int(i1), Int(i2)) ->
				 let v = match op with
				   | Add  -> Int (i1 + i2)
				   | Sub  -> Int (i1 - i2)
				   | Mult -> Int (i1 * i2)
				   | Div  -> Int (i1 / i2)
				   | Mod  -> Int (i1 mod i2)
				   | Eq   -> Bool (i1 = i2)
				   | Neq  -> Bool (i1 != i2)
				   | Lt   -> Bool (i1 < i2)
				   | Le   -> Bool (i1 <= i2)
				   | Gt   -> Bool (i1 > i2)
				   | Ge   -> Bool (i1 >= i2)
				 in v
			    in res
