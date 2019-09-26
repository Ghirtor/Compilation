open CommonAST
module Gto = GotoAST

type expression =
  | Literal  of literal
  | Location of location
  | UnaryOp  of unaryOp  * expression
  | BinaryOp of binaryOp * expression * expression
  | NewBlock of expression
  | FunCall of identifier * expression list

and location =
  | Identifier  of identifier
  | BlockAccess of expression * expression

type instruction = int * instr_descr
and instr_descr =
  | Sequence of instruction * instruction
  | FunCall of identifier * expression list
  | Set of location * expression
  | Label of label
  | Goto of label
  | ConditionalGoto of label * expression
  | Nop
  | Return of expression

type function_info = {
  signature: function_signature;
  code: instruction;
  locals: typ Symb_Tbl.t;
  full_convention: bool;
}

type program = {
  main: instruction;
  globals: typ Symb_Tbl.t;
  structs: struct_type Symb_Tbl.t;
  functions: function_info Symb_Tbl.t;
  size: int;
}
      
let rec index_expression (e : Gto.expression) =
  match e with
  | Literal(x) -> Literal(x)
  | Location(l) -> Location(index_location l)
  | UnaryOp(op, expr) -> UnaryOp(op, index_expression expr)
  | BinaryOp(op, expr1, expr2) -> BinaryOp(op, index_expression expr1, index_expression expr2)
  | NewBlock(expr) -> NewBlock(index_expression expr)
  | FunCall(id, l) -> FunCall(id, List.map (fun elt -> index_expression elt) l)

and index_location (l : Gto.location) =
  match l with
  | Identifier(id) -> Identifier(id)
  | BlockAccess(expr1, expr2) -> BlockAccess(index_expression expr1, index_expression expr2)

let index_instruction (i : Gto.instruction) =
  let rec aux (i : Gto.instruction) num =
    match i with
    | Sequence(instr1, instr2) ->
       let (res1, num1) = aux instr1 num in
       let (res2, num2) = aux instr2 num1 in
       ((num, Sequence(res1, res2)), num2)
    | FunCall(id, l) -> ((num, FunCall(id, List.map (fun elt -> index_expression elt) l)), num+1)
    | Set(loc, expr) -> ((num, Set(index_location loc, index_expression expr)), num+1)
    | Label(l) -> ((num, Label(l)), num+1)
    | Goto(l) -> ((num, Goto(l)), num+1)
    | ConditionalGoto(l, expr) -> ((num, ConditionalGoto(l, index_expression expr)), num+1)
    | Nop -> ((num, Nop), num+1)
    | Return(expr) -> ((num, Return(index_expression expr)), num+1)
  in fst (aux i 0)
    
let index_program (p : Gto.program) =
  (*let goto_fun_list = List.map (fun elt -> snd elt) (Symb_Tbl.bindings p.functions) in*)
  let indexed_functions = Symb_Tbl.map (fun (elt : Gto.function_info) -> { signature = elt.signature; code = index_instruction elt.code; locals = elt.locals; full_convention = elt.full_convention; }) p.functions in
  {
    main = index_instruction p.main;
    globals = p.globals;
    structs = p.structs;
    functions = indexed_functions;
    size = p.size;
  }

let rec strip_expression e =
  match e with
  | Literal(x) -> Gto.Literal(x)
  | Location(l) -> Gto.Location(strip_location l)
  | UnaryOp(op, expr) -> Gto.UnaryOp(op, strip_expression expr)
  | BinaryOp(op, expr1, expr2) -> Gto.BinaryOp(op, strip_expression expr1, strip_expression expr2)
  | NewBlock(expr) -> Gto.NewBlock(strip_expression expr)
  | FunCall(id, l) -> Gto.FunCall(id, List.map (fun elt -> strip_expression elt) l)

and strip_location l =
  match l with
  | Identifier(id) -> Gto.Identifier(id)
  | BlockAccess(expr1, expr2) -> Gto.BlockAccess(strip_expression expr1, strip_expression expr2)

let strip_instruction i =
  let rec aux (_, i) =
    match i with
    | Sequence(instr1, instr2) ->
       let res1 = aux instr1 in
       let res2 = aux instr2 in
       Gto.Sequence(res1, res2)
    | FunCall(id, l) -> Gto.FunCall(id, List.map (fun elt -> strip_expression elt) l)
    | Set(loc, expr) -> Gto.Set(strip_location loc, strip_expression expr)
    | Label(l) -> Gto.Label(l)
    | Goto(l) -> Gto.Goto(l)
    | ConditionalGoto(l, expr) -> Gto.ConditionalGoto(l, strip_expression expr)
    | Nop -> Gto.Nop
    | Return(expr) -> Gto.Return(strip_expression expr)
  in aux i
  
let strip_program (p : program) =
  let not_indexed_functions = Symb_Tbl.map (fun (elt : function_info) -> Gto.{ signature = elt.signature; code = strip_instruction elt.code; locals = elt.locals; full_convention = elt.full_convention; }) p.functions in
  Gto.{
    main = strip_instruction p.main;
    globals = p.globals;
    structs = p.structs;
    functions = not_indexed_functions;
    size = p.size;
  }
