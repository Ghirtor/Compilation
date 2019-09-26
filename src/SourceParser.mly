%{

  (* Contexte *)
  open Lexing
  open CommonAST
  open SourceLocalisedAST

  let array_init loc e sys =
    begin [@ocaml.warning "-8"]
      let remaining = ref true in
      let counter = ref (
	match e.expr with
	| NewArray(e1, e2) -> e1
	| NewArray2(e, t) -> remaining := false; e
      ) in
      let expr = ref (
	match e.expr with
	| NewArray(e1, e2) -> e2.expr
	| NewArray2(e, t) -> remaining := false; e.expr
      ) in
      let l = 0 in (* random line not usefull for syntax error detection because this code is automatically generated *)
      let c = 0 in (* random column not usefull for syntax error detection because this code is automatically generated *)
      let table = ref Symb_Tbl.empty in 
      let rec aux loc e counter sys =
	table := Symb_Tbl.add ("sys_"^(string_of_int (sys))) TypInt !table;
	match e with
	| NewArray(e1, e2) as expr ->
	   let cond = mk_expr (BinaryOp(Lt, mk_expr (Location(Identifier(Id("sys_"^(string_of_int (sys)))))) l c, counter)) l c in
	   let incr = mk_instr (Set(Identifier(Id("sys_"^(string_of_int (sys)))), mk_expr (BinaryOp(Add, mk_expr (Location(Identifier(Id("sys_"^(string_of_int (sys)))))) l c, mk_expr (Literal(Int(1))) l c)) l c)) l c in
	   let init = mk_instr (Set(Identifier(Id("sys_"^(string_of_int (sys)))), mk_expr (Literal(Int(0))) l c)) l c in
	   let blck = mk_instr (Sequence(mk_instr (Set(loc, mk_expr expr l c)) l c, mk_instr (aux (ArrayAccess(mk_expr (Location(loc)) l c, mk_expr (Location(Identifier(Id("sys_"^(string_of_int (sys+1)))))) l c)) e2.expr e1 (sys+1)) l c)) l c in
	   Sequence(init, mk_instr (Loop(cond, blck, incr)) l c)
	| NewArray2(e, t) as expr ->
	   let cond = mk_expr (BinaryOp(Lt, mk_expr (Location(Identifier(Id("sys_"^(string_of_int (sys)))))) l c, counter)) l c in
	   let incr = mk_instr (Set(Identifier(Id("sys_"^(string_of_int (sys)))), mk_expr (BinaryOp(Add, mk_expr (Location(Identifier(Id("sys_"^(string_of_int (sys)))))) l c, mk_expr (Literal(Int(1))) l c)) l c)) l c in
	   let init = mk_instr (Set(Identifier(Id("sys_"^(string_of_int (sys)))), mk_expr (Literal(Int(0))) l c)) l c in
	   let blck = mk_instr (Set(loc, mk_expr expr l c)) l c in
	   Sequence(init, mk_instr (Loop(cond, blck, incr)) l c)
      in if !remaining then (!table, Sequence(mk_instr (Set(loc, e)) l c, mk_instr (aux (ArrayAccess(mk_expr (Location(loc)) l c, mk_expr (Location(Identifier(Id("sys_"^(string_of_int (sys)))))) l c)) !expr !counter sys) l c)) else (!table, Set(loc, e))
    end
    
%}

(* Définition des lexèmes *)
%token <int> CONST_INT
%token <bool> CONST_BOOL
%token <string> IDENT
%token PLUS MINUS STAR DIV MOD
%token EQUAL NEQ LE LT GE GT
%token AND OR NOT
%token LP RP
%token LB RB

%token NEW
%token VAR
%token INTEGER BOOLEAN

%token MAIN
%token STRUCT
%token IF ELSE ELIF WHILE FOR
%token SEMI COMMA DOT
%token SET BREAK CONTINUE INIT RETURN
%token BEGIN END
%token EOF

%left SEMI
%left COMMA
%right SET
%right INIT
%left OR
%left AND
%left NEQ
%left EQUAL
%nonassoc LT
%nonassoc LE
%nonassoc GT
%nonassoc GE
%left PLUS
%left MINUS
%left STAR
%left DIV
%left MOD
%right NOT
%left LP RP LB RB
%left DOT


(* Définition du symbole initial *)
%start prog
%type <SourceLocalisedAST.program> prog

%%

(* Symbole non-terminal principal [prog] *)
prog:
(* Règles : un programme est formé d'une séquence de déclarations de variables
   suivie du bloc de code principal. *)
| decls=var_struct_fun_decls; main=main; EOF
  (* Les déclarations de variables donnent une table des symboles, à laquelle
     est ajoutée la variable spéciale [arg] (avec le type entier). *)
  {
    let l = $startpos.pos_lnum in
    let c = $startpos.pos_cnum - $startpos.pos_bol in
    let (vars, instr, structs, funs) = decls in
    let (table, i) = main in
    let main_signature = { return = TypInt; formals = [("arg", TypInt)]; } in
    let main_call = mk_instr (Set(Identifier(Id("prog_result")), (mk_expr (FunCall(Id("main"), [mk_expr (Location(Identifier(Id("arg")))) l c])) l c))) l c in
    let global_vars = (Symb_Tbl.add "prog_result" TypInt (Symb_Tbl.add "arg" TypInt vars)) in
    if instr.instr = Nop then 
      { main = main_call; globals = global_vars; structs = structs; functions = Symb_Tbl.add "main_integer" ({ signature = main_signature; code = i; locals = table; full_convention = false; }) funs; }
    else
      { main = mk_instr (Sequence(instr, main_call)) l c; globals = global_vars; structs = structs; functions = Symb_Tbl.add "main_integer" ({ signature = main_signature; code = i; locals = table; full_convention = false; }) funs; }
  }
  ;

(*struct_decl:
| { Symb_Tbl.empty }
| STRUCT i=IDENT BEGIN f=field_decl END { Symb_Tbl.singleton i { fields=f; } }
| struct1=struct_decl SEMI struct2=struct_decl
   {
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     Symb_Tbl.union (fun v1 v2 -> failwith(Printf.sprintf "struct %s at line %d and column %d previously declared" v1 ($startpos.pos_lnum) ($startpos.pos_cnum - $startpos.pos_bol))) struct1 struct2
   }
  ;*)

field_decl:
| { [] }
| t=typ i=IDENT { [(i, t)] }
| f1=field_decl SEMI f2=field_decl { f1 @ f2 }
;

formal_params:
| { [] }
| f=formal_params_l { f }
;

formal_params_l:
| t=typ i=IDENT { [(i, t)] }
| t=typ i=IDENT COMMA f=formal_params_l { [(i, t)]@f }
;

var_struct_fun_decls:
| (* empty *)  { (Symb_Tbl.empty, mk_instr Nop $startpos.pos_lnum ($startpos.pos_cnum - $startpos.pos_bol), Symb_Tbl.empty, Symb_Tbl.empty) }
| vars1=var_decl SEMI vars2=var_struct_fun_decls
   {
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     let (vars1, instr1, structs1, fun1) = vars1 in
     let (vars2, instr2, structs2, fun2) = vars2 in
     let table1 = Symb_Tbl.union (fun v1 v2 -> failwith(Printf.sprintf "var %s at line %d and column %d previously declared" v1 ($startpos.pos_lnum) ($startpos.pos_cnum - $startpos.pos_bol))) vars1 vars2 in
     match (instr1.instr, instr2.instr) with
     | (Nop, Nop) -> (table1, mk_instr Nop l c, structs2, fun2)
     | (Nop, _) -> (table1, instr2, structs2, fun2)
     | (_, Nop) -> (table1, instr1, structs2, fun2)
     | (_, _) -> (table1, mk_instr (Sequence(instr1, instr2)) l c, structs2, fun2)
   }
| vars1=struct_decl vars2=var_struct_fun_decls
   {
     let (vars1, instr1, structs1, fun1) = vars1 in
     let (vars2, instr2, structs2, fun2) = vars2 in
     let table2 = Symb_Tbl.union (fun v1 v2 -> failwith(Printf.sprintf "struct %s at line %d and column %d previously declared" v1 ($startpos.pos_lnum) ($startpos.pos_cnum - $startpos.pos_bol))) structs1 structs2 in
     (vars2, instr2, table2, fun2)
   }
| vars1=fun_decl vars2=var_struct_fun_decls
   {
     let (vars1, instr1, structs1, fun1) = vars1 in
     let (vars2, instr2, structs2, fun2) = vars2 in
     let table3 = Symb_Tbl.union (fun v1 v2 -> failwith(Printf.sprintf "function %s at line %d and column %d previously declared" v1 ($startpos.pos_lnum) ($startpos.pos_cnum - $startpos.pos_bol))) fun1 fun2 in
     (vars2, instr2, structs2, table3)
   }
;

(* Séquence de déclaration de variables *)
var_decl:
| VAR t=typ l=multi_decls i=init
   {
     let li = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     let table = List.fold_left (fun table x -> Symb_Tbl.add x t table) Symb_Tbl.empty l in
     match i with
     | [] -> (table, mk_instr Nop li c, Symb_Tbl.empty, Symb_Tbl.empty)
     | x::s ->
	(table, Symb_Tbl.fold (fun k v acc ->
	  if acc.instr = Nop then mk_instr (Set(Identifier(Id(k)), x)) li c
	  else
	    begin
	      let i2 = mk_instr (Set(Identifier(Id(k)), x)) li c in
	      mk_instr (Sequence(acc, i2)) li c
	    end
	) table (mk_instr Nop li c), Symb_Tbl.empty, Symb_Tbl.empty)
   }
;

struct_decl:
| STRUCT i=IDENT BEGIN f=field_decl END { (Symb_Tbl.empty, mk_instr Nop $startpos.pos_lnum ($startpos.pos_cnum - $startpos.pos_bol), Symb_Tbl.singleton i { fields=f; }, Symb_Tbl.empty) }
;

var_decls:
| (* empty *)  { (Symb_Tbl.empty, mk_instr Nop $startpos.pos_lnum ($startpos.pos_cnum - $startpos.pos_bol), Symb_Tbl.empty, (Symb_Tbl.empty, Symb_Tbl.empty)) }
| vars1=var_decl SEMI vars2=var_decls
   {
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     let (vars1, instr1, structs1, fun1) = vars1 in
     let (vars2, instr2, structs2, fun2) = vars2 in
     let table1 = Symb_Tbl.union (fun v1 v2 -> failwith(Printf.sprintf "var %s at line %d and column %d previously declared" v1 ($startpos.pos_lnum) ($startpos.pos_cnum - $startpos.pos_bol))) vars1 vars2 in
     match (instr1.instr, instr2.instr) with
     | (Nop, Nop) -> (table1, mk_instr Nop l c, structs2, fun2)
     | (Nop, _) -> (table1, instr2, structs2, fun2)
     | (_, Nop) -> (table1, instr1, structs2, fun2)
     | (_, _) -> (table1, mk_instr (Sequence(instr1, instr2)) l c, structs2, fun2)
   }

fun_decl:
| t=typ i=IDENT LP f=formal_params RP BEGIN; vars=var_decls b=localised_instruction; END
  {
    let l = $startpos.pos_lnum in
    let c = ($startpos.pos_cnum - $startpos.pos_bol) in
    let (locals, instr, _, _) = vars in
    let signature = { return = t; formals = f; } in
    let (table, bl) = b in
    let infos = { signature = signature; code = mk_instr (Sequence(instr, bl)) l c; locals = Symb_Tbl.union (fun v1 v2 -> failwith (Printf.sprintf "var %s at line %d and column %d previously declared" v1 l c)) table locals; full_convention = false; } in
    let args_str = List.fold_left (fun acc (id, t) -> acc^"_"^(print_typ t)) "" signature.formals in
    (Symb_Tbl.empty, mk_instr Nop l c, Symb_Tbl.empty, Symb_Tbl.singleton (i^args_str) infos)
  }
| i=IDENT LP f=formal_params RP BEGIN; vars=var_decls b=localised_instruction; END
  {
    let l = $startpos.pos_lnum in
    let c = ($startpos.pos_cnum - $startpos.pos_bol) in
    let (locals, instr, _, _) = vars in
    let signature = { return = TypVoid; formals = f; } in
    let (table, bl) = b in
    let infos = { signature = signature; code = mk_instr (Sequence(instr, bl)) l c; locals = Symb_Tbl.union (fun v1 v2 -> failwith (Printf.sprintf "var %s at line %d and column %d previously declared" v1 l c)) table locals; full_convention = false; } in
    let args_str = List.fold_left (fun acc (id, t) -> acc^"_"^(print_typ t)) "" signature.formals in
    (Symb_Tbl.empty, mk_instr Nop l c, Symb_Tbl.empty, Symb_Tbl.singleton (i^args_str) infos)
  }
;

arguments:
| { [] }
| a=arguments_l { a }
;

arguments_l:
| e=localised_expression { [e] }
| e1=localised_expression COMMA a=arguments_l { [e1]@a }
;

elementary_typ:
| INTEGER { TypInt }
| BOOLEAN { TypBool }
;

new_typ:
| t=elementary_typ LB e=localised_expression RB { NewArray2(e, t) }
| t=localised_new_typ LB e=localised_expression RB { NewArray(e, t) }
;

typ:
| e=elementary_typ { e }
| t=typ LB RB { TypArray(t) }
| i=IDENT { TypStruct(i) }
;
  
multi_decls:
| i=IDENT { [i] }
| i1=multi_decls COMMA i2=multi_decls { i1@i2 }
;

init:
| { [] }
| INIT e=localised_expression { [e] }
;
    
(* Bloc de code principal, formé du mot-clé [main] suivi par le bloc
   proprement dit. *)
main:
| MAIN BEGIN; vars=var_decls b=localised_instruction; END {
  let l = $startpos.pos_lnum in
  let c = ($startpos.pos_cnum - $startpos.pos_bol) in
  let (locals, instr, _, _) = vars in
  let (table, bl) = b in
  let v = Symb_Tbl.union (fun v1 v2 -> failwith (Printf.sprintf "var %s at line %d and column %d previously declared" v1 l c)) table locals in
  (v, mk_instr (Sequence(instr, bl)) l c)
}
(*| MAIN; i=block { i }*)
| error {
  let pos = $startpos in
  let l = pos.pos_lnum in
  let c = (pos.pos_cnum - pos.pos_bol) in
  let message =
    Printf.sprintf "Syntax error at %d, %d" l c
  in
  failwith message }
;

(* Un bloc est une instruction ou séquence d'instructions entre accolades. *)
block:
| BEGIN; i=localised_instruction; END { i }
;

loop_block:
| BEGIN; i=localised_loop_instruction; END { i }
;

(* Instruction localisée : on mémorise les numéros de ligne et de colonne du
   début de l'instruction.
   Voir dans la doc la définition de [Lexing.position] pour la signification
   de [pos_lnum], [pos_cnum] et [pos_bol]. *)
localised_instruction:
| i=instruction { let (t, i) = i in
		  let l = $startpos.pos_lnum in
                  let c = $startpos.pos_cnum - $startpos.pos_bol in
                  (t, mk_instr i l c) }
;

localised_loop_instruction:
| i=loop_instruction { let (t, i) = i in
		       let l = $startpos.pos_lnum in
                       let c = $startpos.pos_cnum - $startpos.pos_bol in
                       (t, mk_instr i l c) }
;

localised_expression:
| e=expression { let l = $startpos.pos_lnum in
                  let c = $startpos.pos_cnum - $startpos.pos_bol in
                  mk_expr e l c }
;

localised_new_typ:
| nt=new_typ { let l = $startpos.pos_lnum in
                  let c = $startpos.pos_cnum - $startpos.pos_bol in
                  mk_expr nt l c }
;

init_for:
| id=IDENT SET e=localised_expression
   {
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     mk_instr (Set(Identifier(Id(id)), e)) l c
   }
| { let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     mk_instr Nop l c
   }
| error {
  let pos = $startpos in
  let l = pos.pos_lnum in
  let c = (pos.pos_cnum - pos.pos_bol) in
  let message =
    Printf.sprintf "Syntax error at %d, %d" l c
  in
  failwith message }
;

increment_for:
| id=IDENT SET e=localised_expression
   {
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     mk_instr (Set(Identifier(Id(id)), e)) l c
   }
| { let l = $startpos.pos_lnum in
    let c = $startpos.pos_cnum - $startpos.pos_bol in
    mk_instr Nop l c
  }
| error {
  let pos = $startpos in
  let l = pos.pos_lnum in
  let c = (pos.pos_cnum - pos.pos_bol) in
  let message =
    Printf.sprintf "Syntax error at %d, %d" l c
  in
  failwith message }
;

(* Instructions *)
instruction:
(* Si pas d'instruction, on renvoie l'instruction neutre. *)
| (* empty *)  { (Symb_Tbl.empty, Nop) }
(* Sinon : à compléter ! *)
| i=IDENT LP a=arguments RP { (Symb_Tbl.empty, FunCall(Id(i), a)) }
| RETURN LP e=localised_expression RP { (Symb_Tbl.empty, Return(e)) }
| loc=location SET e=localised_expression
   {
     match e.expr with
     | NewArray(e1, e2) -> array_init loc e 0
     | NewArray2(e1, t) -> array_init loc e 0
     | x -> (Symb_Tbl.empty, Set(loc, e))
   }
| IF LP e=localised_expression RP i=block seq=elif_else_instr { let (t1, i1) = i in let (t2, i2) = seq in (Symb_Tbl.union (fun k v1 v2 -> Some(v1)) t1 t2, Conditional(e, i1, i2)) }
| WHILE LP e=localised_expression RP i=loop_block
   {
     let (t, i) = i in
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     (t, Loop(e, i, mk_instr Nop l c))
   }
| FOR LP init=init_for COMMA condition=localised_expression COMMA increment=increment_for RP i=loop_block
   {
     let (t, i) = i in
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     if (init.instr) = Nop then (t, Loop(condition, i, increment))
     else (t, Sequence(init, (mk_instr (Loop(condition, i, increment)) l c)))
   }
| i1=localised_instruction SEMI i2=localised_instruction { let (t1, i1) = i1 in let (t2, i2) = i2 in (Symb_Tbl.union (fun k v1 v2 -> Some(v1)) t1 t2, Sequence(i1, i2)) }
| error {
  let pos = $startpos in
  let l = pos.pos_lnum in
  let c = (pos.pos_cnum - pos.pos_bol) in
  let message =
    Printf.sprintf "Syntax error at %d, %d" l c
  in
  failwith message }
;

elif_else_instr:
| {
    let l = $startpos.pos_lnum in
    let c = $startpos.pos_cnum - $startpos.pos_bol in
    (Symb_Tbl.empty, mk_instr Nop l c)
  }
| ELSE i=block { i }
| ELIF LP e=localised_expression RP i=block seq=elif_else_instr
   {
     let (t1, i1) = i in
     let (t2, i2) = seq in
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     (Symb_Tbl.union (fun k v1 v2 -> Some(v1)) t1 t2, mk_instr (Conditional(e, i1, i2)) l c)
   }
;
   
(* loop Instructions *)
loop_instruction:
(* Si pas d'instruction, on renvoie l'instruction neutre. *)
| (* empty *)  { (Symb_Tbl.empty, Nop) }
(* Sinon : à compléter ! *)
| BREAK { (Symb_Tbl.empty, Break) }
| CONTINUE { (Symb_Tbl.empty, Continue) }
| i=IDENT LP a=arguments RP { (Symb_Tbl.empty, FunCall(Id(i), a)) }
| RETURN LP e=localised_expression RP { (Symb_Tbl.empty, Return(e)) }
| loc=location SET e=localised_expression
   {
     match e.expr with
     | NewArray(e1, e2) -> array_init loc e 0
     | NewArray2(e1, t) -> array_init loc e 0
     | x -> (Symb_Tbl.empty, Set(loc, e))
   }
| IF LP e=localised_expression RP i=loop_block seq=elif_else_loop_instr { let (t1, i1) = i in let (t2, i2) = seq in (Symb_Tbl.union (fun k v1 v2 -> Some(v1)) t1 t2, Conditional(e, i1, i2)) }
| WHILE LP e=localised_expression RP i=loop_block
   {
     let (t, i) = i in
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     (t, Loop(e, i, mk_instr Nop l c))
   }
| FOR LP init=init_for COMMA condition=localised_expression COMMA increment=increment_for RP i=loop_block
   {
     let (t, i) = i in
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     if (init.instr) = Nop then (t, Loop(condition, i, increment))
     else (t, Sequence(init, (mk_instr (Loop(condition, i, increment)) l c)))
   }
| i1=localised_loop_instruction SEMI i2=localised_loop_instruction { let (t1, i1) = i1 in let (t2, i2) = i2 in (Symb_Tbl.union (fun k v1 v2 -> Some(v1)) t1 t2, Sequence(i1, i2)) }
| error {
  let pos = $startpos in
  let l = pos.pos_lnum in
  let c = (pos.pos_cnum - pos.pos_bol) in
  let message =
    Printf.sprintf "Syntax error at %d, %d" l c
  in
  failwith message }
;

elif_else_loop_instr:
| {
    let l = $startpos.pos_lnum in
    let c = $startpos.pos_cnum - $startpos.pos_bol in
    (Symb_Tbl.empty, mk_instr Nop l c)
  }
| ELSE i=loop_block { i }
| ELIF LP e=localised_expression RP i=loop_block seq=elif_else_loop_instr
   {
     let (t1, i1) = i in
     let (t2, i2) = seq in
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     (Symb_Tbl.union (fun k v1 v2 -> Some(v1)) t1 t2, mk_instr (Conditional(e, i1, i2)) l c)
  }
;
    
expression:
| lit=literal { Literal(lit) }
| loc=location { Location(loc) }
| LP e=expression RP { e }
| NEW i=IDENT { NewRecord(i) }
| NEW nt=new_typ
   {
     let rec dims t acc =
       begin [@ocaml.warning "-8"]
	   match t with
	   | NewArray(e1, e2) -> dims (e2.expr) (e1::acc)
	   | NewArray2(e, t) -> e::acc
       end
     in
     let rec reverse t l =
       begin [@ocaml.warning "-8"]
	   match t.expr with
	   | NewArray(e1, e2) -> { expr=NewArray(List.hd l, reverse e2 (List.tl l)); e_pos=t.e_pos }
	   | NewArray2(e, typ) -> { expr=NewArray2(List.hd l, typ); e_pos=t.e_pos }
       end
     in
     begin [@ocaml.warning "-8"]
	 match nt with
	 | NewArray(e1, e2) ->
	    let dim_list = dims nt [] in
	    NewArray(List.hd dim_list, reverse e2 (List.tl dim_list))
	 | NewArray2(e, t) as expr -> expr
     end
   }
| uop=unop e=localised_expression { UnaryOp(uop, e) }
| e1=localised_expression bop=binop e2=localised_expression { BinaryOp(bop, e1, e2) }
| i=IDENT LP a=arguments RP { FunCall(Id(i), a) }
;

literal:
| c=CONST_INT { Int(c) }
| c=CONST_BOOL { Bool(c) }
;

location:
| id=IDENT { Identifier(Id(id)) }
| e1=localised_expression LB e2=localised_expression RB { ArrayAccess(e1, e2) }
| e=localised_expression DOT i=IDENT { FieldAccess(e, i) }
;

%inline unop:
| MINUS { Minus }
| NOT { Not }
;

%inline binop:
| PLUS { Add }
| MINUS { Sub }
| STAR { Mult }
| DIV { Div }
| MOD { Mod }
| EQUAL { Eq }
| NEQ { Neq }
| LT { Lt }
| LE { Le }
| GT { Gt }
| GE { Ge }
| AND { And }
| OR { Or }
;
   
(*Begin:
| BEGIN {  }
| error { failwith (Printf.sprintf "Syntax error at line %d and column %d, token expected : {" $startpos.pos_lnum ($startpos.pos_cnum - $startpos.pos_bol)) }
;

End:
| END {  }
| error { failwith (Printf.sprintf "Syntax error at line %d and column %d, token expected : }" $startpos.pos_lnum ($startpos.pos_cnum - $startpos.pos_bol)) }
;

Lp:
| LP {  }
| error { failwith (Printf.sprintf "Syntax error at line %d and column %d, token expected : (" $startpos.pos_lnum ($startpos.pos_cnum - $startpos.pos_bol)) }
;

Rp:
| RP {  }
| error { failwith (Printf.sprintf "Syntax error at line %d and column %d, token expected : )" $startpos.pos_lnum ($startpos.pos_cnum - $startpos.pos_bol)) }
;

Comma:
| COMMA {  }
| error { failwith (Printf.sprintf "Syntax error at line %d and %d, token expected : ," $startpos.pos_lnum ($startpos.pos_cnum - $startpos.pos_bol)) }
;

Set:
| SET {  }
| error { failwith (Printf.sprintf "Syntax error at line %d and column %d, token expected : :=" $startpos.pos_lnum ($startpos.pos_cnum - $startpos.pos_bol)) }
  ;*)
