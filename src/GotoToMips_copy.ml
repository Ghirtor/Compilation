open CommonAST
open GotoAST
open Mips

exception Not_found

(* Fonctions auxiliaires fournissant les pseudo-instructions [push] et [pop]. *)
let push reg stack_offset = sw reg stack_offset sp
let pop  reg stack_offset = lw reg stack_offset sp

let print_char () =
  label "print_char" @@ li v0 11 @@ syscall @@ jr ra

let print_string () =
  label "print_string" @@ li v0 4 @@ syscall @@ jr ra

let print_int () =
  label "print_int" @@ li v0 1 @@ syscall @@ jr ra

let sbrk_syscall () =
  label "sbrk_syscall" @@ li v0 9 @@ syscall @@ addi v0 v0 4 @@ sh t0 (-4) v0 @@ jr ra

let sbrk t =
  addi t t 4 @@ move a0 t @@ jal "sbrk_syscall"

let invalid_array_access_1 () =
  label "invalid_array_access_1" @@ la a0 "invalid_array_access_msg1_part1" @@ jal "print_string" @@ move a0 a1 @@ jal "print_int" @@ la a0 "invalid_array_access_msg1_part2" @@ jal "print_string" @@ move a0 a2 @@ jal "print_int" @@ li v0 10 @@ syscall

let invalid_array_access_2 () =
  label "invalid_array_access_2" @@ la a0 "invalid_array_access_msg2" @@ jal "print_string" @@ li v0 10 @@ syscall

let invalid_array_size () =
  label "invalid_array_size" @@ la a0 "invalid_array_size_msg" @@ jal "print_string" @@ li v0 10 @@ syscall

let array_not_initialized () =
  label "array_not_initialized" @@ la a0 "array_not_initialized_msg" @@ jal "print_string" @@ li v0 10 @@ syscall

let integer_out_range () =
  label "integer_out_range" @@ la a0 "integer_out_range_msg" @@ jal "print_string" @@ li v0 10 @@ syscall
    
(**
   Fonction de traduction des expressions.
   [translate_expression : GotoAST.expression -> Mips.text]

   Rappel de la convention : le code généré par [translate_expression e] doit
   placer la valeur de l'expression [e] au sommet de la pile.
*)
let rec translate_expression (e: GotoAST.expression) stack_offset =
  begin [@ocaml.warning "-8"]
      match e with
      | Literal(Int(i)) -> ((li t0 i @@ push t0 stack_offset), stack_offset-4)
      | Literal(Bool(b)) -> (((if b then li t0 1 else li t0 0) @@ push t0 stack_offset), stack_offset-4)
      | Location(x) as l -> translate_location l stack_offset
      | NewBlock(e) ->
	 let (e, stack_offset) = translate_expression e stack_offset in
	 (e @@ pop t0 (stack_offset+4) @@ blez t0 "invalid_array_size" @@ li t1 4 @@ mul t1 t0 t1 @@ sbrk t1 @@ push v0 (stack_offset+4), stack_offset)
      | UnaryOp(op, e) ->
	 let (e, stack_offset) = translate_expression e stack_offset in
	 ((e @@ pop t0 (stack_offset+4) @@ (match op with
	 | Minus ->
            neg t0 t0
	 | Not ->
	    not_ t0 t0)
	   @@ push t0 (stack_offset+4)), stack_offset)
      | BinaryOp((Add|Sub|Lt as op), e1, Literal(Int(i))) ->
	 let (e1, stack_offset) = translate_expression e1 stack_offset in
	 ((e1 @@ pop t0 (stack_offset+4) @@ (match op with
	 | Add -> addi t0 t0 i
	 | Sub -> subi t0 t0 i
	 | Lt -> slti t0 t0 i)
	   @@ push t0 (stack_offset+4)), stack_offset)
      | BinaryOp((Add|Sub as op), Literal(Int(i)), e2) ->
	 let (e2, stack_offset) = translate_expression e2 stack_offset in
	 ((e2 @@ pop t1 (stack_offset+4) @@ (match op with
	 | Add -> addi t0 t1 i
	 | Sub -> subi t0 t1 i @@ neg t0 t0)
	   @@ push t0 (stack_offset+4)), stack_offset)
      | BinaryOp((And|Or as op), e1, Literal(Bool(i))) ->
	 let (e1, stack_offset) = translate_expression e1 stack_offset in
	 ((e1 @@ pop t0 (stack_offset+4) @@ (match op with
	 | And -> andi_ t0 t0 (if i then 1 else 0)
	 | Or -> ori_ t0 t0 (if i then 1 else 0))
	   @@ push t0 (stack_offset+4)), stack_offset)
      | BinaryOp((And|Or as op), Literal(Bool(i)), e2) ->
	 let (e2, stack_offset) = translate_expression e2 stack_offset in
	 ((e2 @@ pop t1 (stack_offset+4) @@ (match op with
	 | And -> andi_ t0 t1 (if i then 1 else 0)
	 | Or -> ori_ t0 t1 (if i then 1 else 0))
	   @@ push t0 (stack_offset+4)), stack_offset)
      | e -> translate_binaryOp_no_imm e stack_offset
  end

and translate_location l stack_offset =
  begin [@ocaml.warning "-8"]
      match l with
      | Location(Identifier(Id(id))) -> ((la t0 id @@ lw t0 0 t0 @@ push t0 stack_offset), stack_offset-4)
      | Location(BlockAccess(l, e)) ->
	 let (instructions, stack_offset1) = translate_location l stack_offset in
	 let (e, stack_offset2) = translate_expression e stack_offset1 in
	 (instructions @@ e @@ pop t0 (stack_offset2+4) @@ bltz t0 "invalid_array_access_2" @@ pop t1 (stack_offset1+4) @@ blez t1 "array_not_initialized" @@ lh t2 (-4) t1 @@ move a1 t2 @@ move a2 t0 @@ sub t2 t2 t0 @@ blez t2 "invalid_array_access_1" @@ li t2 4 @@ mul t0 t0 t2 @@ add t1 t0 t1 @@ lw t0 0 t1 @@ push t0 stack_offset, stack_offset-4)
  end

and translate_binaryOp_no_imm (e: GotoAST.expression) stack_offset =
  begin [@ocaml.warning "-8"]
      match e with
      | BinaryOp(op, e1, e2) ->
	 let (e1, stack_offset1) = translate_expression e1 stack_offset in
	 let (e2, stack_offset2) = translate_expression e2 stack_offset1 in
	 ((e1 @@ e2 @@ pop t1 (stack_offset2+4) @@ pop t0 (stack_offset2+8) @@ (match op with
	 | Add -> add t0 t0 t1
	 | Sub -> sub t0 t0 t1
	 | Div -> div t0 t0 t1
	 | Mult -> mul t0 t0 t1
	 | Mod -> rem t0 t0 t1
	 | Eq -> seq t0 t0 t1
	 | Neq -> sne t0 t0 t1
	 | And -> and_ t0 t0 t1
	 | Or -> or_ t0 t0 t1
	 | Lt -> slt t0 t0 t1
	 | Le -> sle t0 t0 t1
	 | Gt -> sgt t0 t0 t1
	 | Ge -> sge t0 t0 t1)
	   @@ push t0 (stack_offset2+8)), stack_offset2+4)
  end

(**
   Fonction de traduction des instructions.
   [translate_instruction : GotoAST.instruction -> Mips.text]
*)
let rec translate_instruction (i: GotoAST.instruction) stack_offset =
  begin [@ocaml.warning "-8"]
      match i with
      | Nop -> (nop, stack_offset)
      | Sequence(i1, i2) ->
	 let (i1, stack_offset1) = translate_instruction i1 stack_offset in
	 let (i2, stack_offset2) = translate_instruction i2 stack_offset1 in
	 ((i1 @@ i2), stack_offset2)
      | Print(e) ->
	 let (e, stack_offset) = translate_expression e stack_offset in
	 ((e @@ pop a0 (stack_offset+4) @@ jal "print_char"), stack_offset+4)
      | Set(Identifier(Id(l)), e) ->
	 let (e, stack_offset) = translate_expression e stack_offset in
	 ((e @@ pop t0 (stack_offset+4) @@ la t1 l @@ sw t0 0 t1), stack_offset+4)
      | Set(BlockAccess(l, e2), e) ->
	 let (e, stack_offset1) = translate_expression e stack_offset in
	 let (addr, stack_offset2) = translate_location l stack_offset1 in
	 let (e2, stack_offset3) = translate_expression e2 stack_offset2 in
	 ((e @@ addr @@ e2 @@ pop t2 (stack_offset3+4) @@ bltz t2 "invalid_array_access_2" @@ pop t0 (stack_offset2+4) @@ blez t0 "array_not_initialized" @@ lh t3 (-4) t0 @@ pop t1 (stack_offset1+4) @@ move a1 t3 @@ move a2 t2 @@ sub t3 t3 t2 @@ blez t3 "invalid_array_access_1" @@ li t3 4 @@ mul t2 t2 t3 @@ add t0 t0 t2 @@ sw t1 0 t0), stack_offset1+4)
      | Label(Lab(l)) -> ((label(l)), stack_offset)
      | Goto(Lab(l)) -> ((b l), stack_offset)
      | ConditionalGoto(Lab(l), e) ->
	 let (e, stack_offset) = translate_expression e stack_offset in
	 ((e @@ pop t0 (stack_offset+4) @@ bnez t0 l), stack_offset+4)
  end

(** 
    Fonction de traduction des programmes
    [translate_program : GotoAST.program -> Mips.program]

    Rien à changer dans cette fonction, elle fournit déjà l'infrastructure dans
    laquelle insérer le code principal.
*)
let translate_program program =
  (* Initialisation : lit le paramètre donné en entrée et enregistre le résultat
     dans les données statiques sous l'étiquette [arg].
     À défaut de paramètre, [arg] vaudra zéro. *)
  let init =
    beqz a0 "init_end"
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ la t0 "arg"
    @@ sw v0 0 t0
    @@ label "init_end"
      
  (* Terminaison du programme avec l'appel système [exit] *)
  and close =
    li v0 10
    @@ syscall

  (* Fonctions prédéfinies.
     En l'occurrence, fonction de lecture du paramètre d'entrée. *)
  and built_ins =
    (* Le paramètre est donné sous la forme d'une chaîne de caractères
       terminée par le caractère [000]. *)
    label "atoi"
      
    (* Variables *)
    @@ move t0 a0 (* t0 : adresse du caractère à lire *)
    @@ li   t1 0  (* t1 : accumulateur pour la valeur calculée *)
    (* On garde t2 pour des calculs intermédiaires *)
      
    (* Constantes *)
    @@ li   t3 10 (* Base décimale *)
    @@ li   t4 48 (* Code ASCII caractère '0' *)
    @@ li   t5 57 (* Code ASCII caractère '9' *)

    (* Début de la boucle de lecture *)
    @@ label "atoi_loop"
    @@ lbu  t2 0 t0 (* Lecture d'un octet *)

    (* Conditions d'arrêt et d'erreur *)
    @@ beq  t2 zero "atoi_end" (* Fin si lecture de [000] *)
    @@ blt  t2 t4 "atoi_error" (* Erreur si caractère non compris entre 0 et 9 *)
    @@ bgt  t2 t5 "atoi_error"

    (* Mise à jour de l'accumulateur *)
    @@ addi t2 t2 (-48) (* Conversion caractère en nombre *)
    @@ mul  t1 t1 t3
    @@ add  t1 t1 t2 (* t1 <- 10 * t1 + t2 *)

    (* Suite de la lecture *)
    @@ addi t0 t0 1
    @@ b "atoi_loop"

    (* Arrêt du programme en cas d'erreur de lecture *)
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall

    (* Renvoi du résultat via [v0] en cas de succès *)
    @@ label "atoi_end"
    @@ move v0 t1
    @@ jr   ra
  in

  (* Construction du texte du programme *)
  let (main_code, _) = translate_instruction program.main 0 in
  let text = init @@ main_code @@ close @@ (print_char () @@ sbrk_syscall () @@ print_string () @@ invalid_array_access_1 () @@ invalid_array_access_2 () @@ invalid_array_size () @@ print_int ()
					    @@ array_not_initialized ()) @@ built_ins in

  (* Initialisation de la partie des données statiques *)
  let data = (Symb_Tbl.fold
    (fun var _ code -> label var @@ dword [0] @@ code)
    program.globals nop) @@ label "invalid_array_access_msg1_part1" @@ asciiz "\ninvalid array access : size is " @@ label "invalid_array_access_msg1_part2" @@ asciiz " but found "
    @@ label "invalid_array_access_msg2" @@ asciiz "\ninvalid array access : negative argument found but positive one expected"
    @@ label "invalid_array_size_msg" @@ asciiz "\ninvalid array size : expected a positive not null argument"
    @@ label "array_not_initialized_msg" @@ asciiz "\narray not initialized"
    @@ label "integer_out_range_msg" @@ asciiz "\ninteger out of range"
  in

  (* Programme généré *)
  { text; data }