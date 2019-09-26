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

let print_int_err () =
  label "print_int_err" @@ li v0 1 @@ syscall @@ jr ra

let sbrk_syscall =
  li v0 9
  @@ syscall
  @@ addi v0 v0 4
  @@ sh t0 (-4) v0
    
let sbrk t =
  addi t t 4 @@ move s6 a0 @@ move a0 t @@ sbrk_syscall @@ move a0 s6

let invalid_array_access_1 () =
  label "invalid_array_access_1" @@ la a0 "invalid_array_access_msg1_part1" @@ jal "print_string" @@ move a0 a1 @@ jal "print_int_err" @@ la a0 "invalid_array_access_msg1_part2" @@ jal "print_string" @@ move a0 a2 @@ jal "print_int_err" @@ li v0 10 @@ syscall

let invalid_array_access_2 () =
  label "invalid_array_access_2" @@ la a0 "invalid_array_access_msg2" @@ jal "print_string" @@ li v0 10 @@ syscall

let invalid_array_size () =
  label "invalid_array_size" @@ la a0 "invalid_array_size_msg" @@ jal "print_string" @@ li v0 10 @@ syscall

let array_not_initialized () =
  label "array_not_initialized" @@ la a0 "array_not_initialized_msg" @@ jal "print_string" @@ li v0 10 @@ syscall

let integer_out_range () =
  label "integer_out_range" @@ la a0 "integer_out_range_msg" @@ jal "print_string" @@ li v0 10 @@ syscall

let check_integer_range t =
  li t1 integer_max_val @@ slt t2 t1 t @@ bgtz t2 "integer_out_range" @@ li t1 integer_min_val @@ slt t2 t t1 @@ bgtz t2 "integer_out_range"

let return_random_val full_convention =
  if full_convention then
    li v0 0
    @@ move sp fp
    @@ lw t1 0 fp
    @@ lw t2 (-4) fp
    @@ lw t3 (-8) fp
    @@ lw t4 (-12) fp
    @@ lw t5 (-16) fp
    @@ lw t6 (-20) fp
    @@ move fp t1
    @@ move ra t2
    @@ move a0 t3
    @@ move a1 t4
    @@ move a2 t5
    @@ move a3 t6
    @@ jr ra
  else
    li v0 0
    @@ move sp fp
    @@ lw t1 0 fp
    @@ lw t2 (-4) fp
    @@ lw t3 (-8) fp
    @@ lw t4 (-12) fp
    @@ lw t5 (-16) fp
    @@ move fp t1
    @@ move a0 t2
    @@ move a1 t3
    @@ move a2 t4
    @@ move a3 t5
    @@ jr ra

let find_arg_register i =
  match i with
  | (-12) -> a0
  | (-8) -> a1
  | (-4) -> a2
  | (0) -> a3
  | _ -> failwith ("argument is not in a register")
    
(**
   Fonction de traduction des expressions.
   [translate_expression : GotoAST.expression -> Mips.text]

   Rappel de la convention : le code généré par [translate_expression e] doit
   placer la valeur de l'expression [e] au sommet de la pile.
*)
let rec translate_expression (e: GotoAST.expression) stack_offset args locals full_convention =
  begin [@ocaml.warning "-8"]
      match e with
      | Literal(Int(i)) -> ((li t0 i @@ (check_integer_range t0) @@ push t0 stack_offset), stack_offset-4)
      | Literal(Bool(b)) -> (((if b then li t0 1 else li t0 0) @@ push t0 stack_offset), stack_offset-4)
      | Location(x) as l -> translate_location l stack_offset args locals full_convention
      | NewBlock(e) ->
	 let (e, stack_offset) = translate_expression e stack_offset args locals full_convention in
	 (e @@ pop t0 (stack_offset+4) @@ blez t0 "invalid_array_size" @@ li t1 4 @@ mul t1 t0 t1 @@ sbrk t1 @@ push v0 (stack_offset+4), stack_offset)
      | UnaryOp(op, e) ->
	 let (e, stack_offset) = translate_expression e stack_offset args locals full_convention in
	 ((e @@ pop t0 (stack_offset+4) @@ (match op with
	 | Minus ->
            neg t0 t0 @@ (check_integer_range t0)
	 | Not ->
	    not_ t0 t0)
	   @@ push t0 (stack_offset+4)), stack_offset)
      | BinaryOp((Add|Sub|Lt as op), e1, Literal(Int(i))) ->
	 let (e1, stack_offset) = translate_expression e1 stack_offset args locals full_convention in
	 ((e1 @@ pop t0 (stack_offset+4) @@ (match op with
	 | Add -> addi t0 t0 i @@ (check_integer_range t0)
	 | Sub -> subi t0 t0 i @@ (check_integer_range t0)
	 | Lt -> slti t0 t0 i)
	   @@ push t0 (stack_offset+4)), stack_offset)
      | BinaryOp((Add|Sub as op), Literal(Int(i)), e2) ->
	 let (e2, stack_offset) = translate_expression e2 stack_offset args locals full_convention in
	 ((e2 @@ pop t1 (stack_offset+4) @@ (match op with
	 | Add -> addi t0 t1 i @@ (check_integer_range t0)
	 | Sub -> subi t0 t1 i @@ neg t0 t0 @@ (check_integer_range t0))
	   @@ push t0 (stack_offset+4)), stack_offset)
      | BinaryOp((And|Or as op), e1, Literal(Bool(i))) ->
	 let (e1, stack_offset) = translate_expression e1 stack_offset args locals full_convention in
	 ((e1 @@ pop t0 (stack_offset+4) @@ (match op with
	 | And -> andi_ t0 t0 (if i then 1 else 0)
	 | Or -> ori_ t0 t0 (if i then 1 else 0))
	   @@ push t0 (stack_offset+4)), stack_offset)
      | BinaryOp((And|Or as op), Literal(Bool(i)), e2) ->
	 let (e2, stack_offset) = translate_expression e2 stack_offset args locals full_convention in
	 ((e2 @@ pop t1 (stack_offset+4) @@ (match op with
	 | And -> andi_ t0 t1 (if i then 1 else 0)
	 | Or -> ori_ t0 t1 (if i then 1 else 0))
	   @@ push t0 (stack_offset+4)), stack_offset)
      | FunCall(Id(i), l) ->
	 let f elt (instr, so) =
	   let (e, stack_offset) = translate_expression elt so args locals full_convention in
	   let instr = match instr with
	     | None -> e
	     | Some(x) -> x @@ e
	   in
	   (Some(instr), stack_offset)
	 in
	 let res = List.fold_right f l (None, stack_offset) in
	 let res2 = match res with
	   | (None, so) -> (addi sp sp stack_offset @@ sw a0 (-8) sp @@ sw a1 (-12) sp @@ sw a2 (-16) sp @@ sw a3 (-20) sp @@ jal i @@ subi sp sp stack_offset @@ push v0 stack_offset, stack_offset-4)
	   | (Some(x), so) ->
	      let size = List.length l in
	      let to_transfer = min 4 size in
	      let copy_val = ref (None, None) in
	      let count = ref 0 in
	      let match_register () =
		match !count with
		| 0 -> (a0, t0)
		| 1 -> (a1, t1)
		| 2 -> (a2, t2)
		| 3 -> (a3, t3)
		| _ -> failwith ("impossible case")
	      in
	      for i = to_transfer-1 downto 0 do
		let (reg1, reg2) = match_register () in
		incr count;
		match !copy_val with
		| (None, None) -> copy_val := (Some(lw reg2 (-4*i) sp), Some(move reg1 reg2))
		| (Some(tmp1), Some(tmp2)) -> copy_val := (Some(tmp1 @@ lw reg2 (-4*i) sp) , Some(tmp2 @@ move reg1 reg2))
	      done;
	      match !copy_val with
	      | (None, None) -> failwith ("impossible case")
	      | (Some(tmp1), Some(tmp2)) -> 
		 (x @@ addi sp sp (so+(to_transfer*4)) @@ tmp1 @@ sw a0 (-8) sp @@ sw a1 (-12) sp @@ sw a2 (-16) sp @@ sw a3 (-20) sp @@ tmp2 @@ jal i @@ subi sp sp (so+(to_transfer*4)) @@ push v0 stack_offset, stack_offset-4)
	 in res2
      | e -> translate_binaryOp_no_imm e stack_offset args locals full_convention
  end

and translate_location l stack_offset args locals full_convention =
  begin [@ocaml.warning "-8"]
      match l with
      | Location(Identifier(Id(id))) ->
	 let offset_local =
	   try
	     if full_convention then
	       (-4 * (fst (List.find (fun elt -> String.equal (fst (snd elt)) id) locals))) - 24
	     else
	       (-4 * (fst (List.find (fun elt -> String.equal (fst (snd elt)) id) locals))) - 20
	   with
	   | _ -> 1
	 in
	 let ind_arg =
	   try
	     (4 * (fst (List.find (fun elt -> String.equal (fst (snd elt)) id) args))) - 16
	   with
	   | _ -> -1
	 in
	 let load_instr =
	   if offset_local <= 0 then lw t0 offset_local fp
	   else
	     begin
	       if ind_arg > 0 then lw t0 ind_arg fp (* value of the var is in stack *)
	       else
		 begin
		   if ind_arg == -1 then (* var is not an argument *)
		     la t0 id @@ lw t0 0 t0 @@ li t1 1 @@ and_ t1 t0 t1 @@ move t2 t0 @@ slti t4 t2 0 @@ srl t2 t2 1 @@ li t3 2147483648 (* 2^31 *)@@ xor t5 t2 t3 @@ movn t2 t5 t4 @@ movz t0 t2 t1 @@ subi t2 t0 1 @@ movn t0 t2 t1
		   else (* value of var is in register a0 - a3 *)
		     begin
		       let reg = find_arg_register ind_arg in
		       move t0 reg
		     end
		 end
	     end
	 in
	 ((load_instr @@ push t0 stack_offset), stack_offset-4)
      | Location(BlockAccess(l, e)) ->
	 let (instructions, stack_offset1) = translate_location l stack_offset args locals full_convention in
	 let (e, stack_offset2) = translate_expression e stack_offset1 args locals full_convention in
	 (instructions @@ e @@ pop t0 (stack_offset2+4) @@ bltz t0 "invalid_array_access_2" @@ pop t1 (stack_offset1+4) @@ blez t1 "array_not_initialized" @@ lh t2 (-4) t1 @@ move a1 t2 @@ move a2 t0 @@ sub t2 t2 t0 @@ blez t2 "invalid_array_access_1" @@ li t2 4 @@ mul t0 t0 t2 @@ add t1 t0 t1 @@ lw t0 0 t1 @@ li t1 1 @@ and_ t1 t0 t1 @@ move t2 t0 @@ slti t4 t2 0 @@ srl t2 t2 1 @@ li t3 2147483648 (* 2^31 *)@@ xor t5 t2 t3  @@ movn t2 t5 t4 @@ movz t0 t2 t1 @@ subi t2 t0 1 @@ movn t0 t2 t1 @@ push t0 stack_offset, stack_offset-4)
  end

and translate_binaryOp_no_imm (e: GotoAST.expression) stack_offset args locals full_convention =
  begin [@ocaml.warning "-8"]
      match e with
      | BinaryOp(op, e1, e2) ->
	 let (e1, stack_offset1) = translate_expression e1 stack_offset args locals full_convention in
	 let (e2, stack_offset2) = translate_expression e2 stack_offset1 args locals full_convention in
	 ((e1 @@ e2 @@ pop t1 (stack_offset2+4) @@ pop t0 (stack_offset2+8) @@ (match op with
	 | Add -> add t0 t0 t1 @@ (check_integer_range t0)
	 | Sub -> sub t0 t0 t1 @@ (check_integer_range t0)
	 | Div -> div t0 t0 t1 @@ (check_integer_range t0)
	 | Mult -> mul t0 t0 t1 @@ (check_integer_range t0)
	 | Mod -> rem t0 t0 t1 @@ (check_integer_range t0)
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
let rec translate_instruction (i: GotoAST.instruction) stack_offset args full_convention locals fun_tab add_to_labels =
  begin [@ocaml.warning "-8"]
      match i with
      | Nop -> (nop, stack_offset)
      | Sequence(i1, i2) ->
	 let (i1, stack_offset1) = translate_instruction i1 stack_offset args full_convention locals fun_tab add_to_labels in
	 let (i2, stack_offset2) = translate_instruction i2 stack_offset1 args full_convention locals fun_tab add_to_labels in
	 ((i1 @@ i2), stack_offset2)
      | FunCall(Id(i), l) ->
	 let (e, stack_offset) = translate_expression (FunCall(Id(i), l)) stack_offset args locals full_convention in
	 (e, stack_offset+4)
      | Return(e) ->
	 let (tail_rec, jump, list, next_fun) =
	   match e with
	   | FunCall(Id(i), l) -> (true, i^"_tail_rec_begin", l, i)
	   | _ -> (false, "", [], "")
	 in
	 let (e, stack_offset) = translate_expression e stack_offset args locals full_convention in
	 let (res, stack_offset2)  = if full_convention then (e @@ pop v0 (stack_offset+4) @@ move sp fp @@ lw t1 0 fp @@ lw t2 (-4) fp @@ lw t3 (-8) fp @@ lw t4 (-12) fp @@ lw t5 (-16) fp @@ lw t6 (-20) fp @@ move fp t1 @@ move ra t2 @@ move a0 t3 @@ move a1 t4 @@ move a2 t5 @@ move a3 t6 @@ jr ra, 0)
	   else (e @@ pop v0 (stack_offset+4) @@ move sp fp @@ lw t1 0 fp @@ lw t2 (-4) fp @@ lw t3 (-8) fp @@ lw t4 (-12) fp @@ lw t5 (-16) fp @@ move fp t1 @@ move a0 t2 @@ move a1 t3 @@ move a2 t4 @@ move a3 t5@@ jr ra, 0)
	 in
	 if tail_rec then
	   begin
	     let f elt (instr, so) =
	       let (e, stack_offset) = translate_expression elt so args locals full_convention in
	       let instr = match instr with
		 | None -> e
		 | Some(x) -> x @@ e
	       in
	       (Some(instr), stack_offset)
	     in
	     let res2 = List.fold_right f list (None, 0) in
	     let res3 = match res2 with
	       | (None, so) -> (move sp fp @@ addi sp sp (max (-so-16) 0) @@ jal jump, 0)
	       | (Some(x), so) ->
		  let copy_val = ref None in
		  let transfer_args i count =
		    lw t3 (-i*4) sp @@ (match !count with
		    | 0 -> move a0 t3
		    | 1 -> move a1 t3
		    | 2 -> move a2 t3
		    | 3 -> move a3 t3
		    | _ -> sw t3 (-i*4) t0)
		  in
		  let count = ref 0 in
		  for i = ((-so)/4)-1 downto 0 do
		    let () = match !copy_val with
		      | None -> copy_val := (Some(transfer_args i count))
		      | Some(tmp) -> copy_val := (Some(tmp @@ (transfer_args i count)))
		    in
		    incr count;
		    ()
		  done;
		  let next_locals = Symb_Tbl.bindings ((Symb_Tbl.find next_fun fun_tab).locals) in
		  match !copy_val with
		  | None -> (move sp fp @@ addi sp sp (max (-so-16) 0) @@ jal jump, 0)
		  | Some(move_args) ->
		     (move sp fp @@ addi sp sp (-24-(4*(List.length next_locals))) @@ x @@ addi t0 fp (max (4*(List.length args)-16) 0) @@ lw t1 0 fp @@ lw t2 (-4) fp @@ lw t3 (-8) fp @@ lw t4 (-12) fp @@ lw t5 (-16) fp @@ lw t6 (-20) fp @@ move_args @@ addi sp t0 (min 0 (so+16)) @@ sw t1 0 sp @@ sw t2 (-4) sp @@ sw t3 (-8) sp @@ sw t4 (-12) sp @@ sw t5 (-16) sp @@ sw t6 (-20) sp @@ jal jump, 0)
	     in res3
	   end
	 else (res, stack_offset2)
      | Set(Identifier(Id(l)), expr) ->
	 let offset_local =
	   try
	     if full_convention then
	       (-4 * (fst (List.find (fun elt -> String.equal (fst (snd elt)) l) locals))) - 24
	     else
	       (-4 * (fst (List.find (fun elt -> String.equal (fst (snd elt)) l) locals))) - 20
	   with
	   | _ -> 1
	 in
	 let ind_arg =
	   try
	     (4 * (fst (List.find (fun elt -> String.equal (fst (snd elt)) l) args))) - 16
	   with
	   | _ -> -1
	 in
	 let (e, stack_offset) = translate_expression expr stack_offset args locals full_convention in
	 let res =
	   if offset_local <= 0 then (e @@ pop t0 (stack_offset+4) @@ sw t0 offset_local fp, stack_offset+4)
	   else
	     begin
	       if ind_arg < 0 then
		 begin
		   if ind_arg == -1 then
		     ((e @@ pop t0 (stack_offset+4) @@ la t1 l @@
			 (match expr with
			 | NewBlock(_) -> addi t0 t0 1 @@ sw t0 0 t1
			 | _ -> lw t2 0 t1 @@ andi_ t3 t2 1 @@ sll t4 t0 1 @@ addi t5 t0 1 @@ movz t0 t4 t3 @@ movn t0 t5 t3 @@ sw t0 0 t1)), stack_offset+4)
		   else
		     let reg = find_arg_register ind_arg in
		     (e @@ pop t0 (stack_offset+4) @@ move reg t0, stack_offset+4)
		 end
	       else
		 (e @@ pop t0 (stack_offset+4) @@ sw t0 ind_arg fp, stack_offset+4)
	     end
	 in res
      | Set(BlockAccess(l, e2), expr) ->
	 let (e, stack_offset1) = translate_expression expr stack_offset args locals full_convention in
	 let (addr, stack_offset2) = translate_location l stack_offset1 args locals full_convention in
	 let (e2, stack_offset3) = translate_expression e2 stack_offset2 args locals full_convention in
	 ((e @@ addr @@ e2 @@ pop t2 (stack_offset3+4) @@ bltz t2 "invalid_array_access_2" @@ pop t0 (stack_offset2+4) @@ blez t0 "array_not_initialized" @@ lh t3 (-4) t0 @@ pop t1 (stack_offset1+4) @@ move a1 t3 @@ move a2 t2 @@ sub t3 t3 t2 @@ blez t3 "invalid_array_access_1" @@ li t3 4 @@ mul t2 t2 t3 @@ add t0 t0 t2 @@
	     (match expr with
	     | NewBlock(_) -> addi t1 t1 1@@ sw t1 0 t0
	     | _ -> lw t5 0 t0 @@ andi_ t6 t5 1 @@ sll t7 t1 1 @@ addi t8 t1 1 @@ movz t1 t7 t6 @@ movn t1 t8 t6 @@ sw t1 0 t0)), stack_offset1+4)
      | Label(Lab(l)) -> ((label(if add_to_labels then l^"_tail_rec" else l)), stack_offset)
      | Goto(Lab(l)) -> ((b (if add_to_labels then l^"_tail_rec" else l)), stack_offset)
      | ConditionalGoto(Lab(l), e) ->
	 let (e, stack_offset) = translate_expression e stack_offset args locals full_convention in
	 ((e @@ pop t0 (stack_offset+4) @@ bnez t0 (if add_to_labels then l^"_tail_rec" else l)), stack_offset+4)
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
    @@ (check_integer_range v0)
    @@ sll v0 v0 1
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

    @@ comment "printInt"
    @@ label "printInt_integer"
    @@ sw fp 0 sp
    @@ sw ra (-4) sp
    @@ label "printInt_integer_tail_rec_begin"
    @@ move fp sp
    @@ li v0 1
    @@ syscall
    @@ lw t1 0 fp
    @@ lw t2 (-4) fp
    @@ lw t3 (-8) fp
    @@ lw t4 (-12) fp
    @@ lw t5 (-16) fp
    @@ lw t6 (-20) fp
    @@ move fp t1
    @@ move ra t2
    @@ move v0 a0
    @@ move a0 t3
    @@ move a1 t4
    @@ move a2 t5
    @@ move a3 t6
    @@ jr ra

    @@ comment "print"
    @@ label "print_integer"
    @@ sw fp 0 sp
    @@ sw ra (-4) sp
    @@ label "print_integer_tail_rec_begin"
    @@ move fp sp
    @@ li v0 11
    @@ syscall
    @@ lw t1 0 fp
    @@ lw t2 (-4) fp
    @@ lw t3 (-8) fp
    @@ lw t4 (-12) fp
    @@ lw t5 (-16) fp
    @@ lw t6 (-20) fp
    @@ move fp t1
    @@ move ra t2
    @@ move v0 a0
    @@ move a0 t3
    @@ move a1 t4
    @@ move a2 t5
    @@ move a3 t6
    @@ jr ra
      
    @@ comment "power"
    @@ label "power_integer_integer"
    @@ sw fp 0 sp
    @@ sw ra (-4) sp
    @@ label "power_integer_integer_tail_rec_begin"
    @@ move fp sp
    @@ move s0 a1
    @@ move s1 a0
    @@ li t0 1
    @@ b "power_loop_guard"
    @@ label "power_loop_code"
    @@ mul t0 t0 s1
    @@ subi s0 s0 1
    @@ label "power_loop_guard"
    @@ bgtz s0 "power_loop_code"
    @@ subi sp sp 4
    @@ lw t1 0 fp
    @@ lw t2 (-4) fp
    @@ lw t3 (-8) fp
    @@ lw t4 (-12) fp
    @@ lw t5 (-16) fp
    @@ lw t6 (-20) fp
    @@ move v0 t0
    @@ move fp t1
    @@ move ra t2
    @@ move a0 t3
    @@ move a1 t4
    @@ move a2 t5
    @@ move a3 t6
    @@ jr ra
  in

  (* Construction du texte du programme *)
  let (main_code, _) = translate_instruction program.main 0 [] false [] program.functions false in
  let fun_list = Symb_Tbl.bindings program.functions in
  let fun_code =
    List.fold_left (fun acc (k, v) ->
      let (instr, so) = (translate_instruction v.code 0 (List.mapi (fun i elt -> (i+1, elt)) v.signature.formals) v.full_convention (List.mapi (fun i elt -> (i, elt)) (Symb_Tbl.bindings v.locals)) program.functions) false in
      match acc with
      | None ->
	 Some(
	   if not v.full_convention then
	     label k
	     @@ sw fp 0 sp
	     @@ move fp sp
	     @@ subi sp sp (20+(4*(List.length (Symb_Tbl.bindings v.locals))))
	     @@ instr
	     @@ (return_random_val v.full_convention)
	     @@ label (k^"_tail_rec_begin")
	     @@ move fp sp
	     @@ subi sp sp (24+(4*(List.length (Symb_Tbl.bindings v.locals))))
	     @@ (fst (translate_instruction v.code 0 (List.mapi (fun i elt -> (i+1, elt)) v.signature.formals) true (List.mapi (fun i elt -> (i, elt)) (Symb_Tbl.bindings v.locals)) program.functions true))
	     @@ (return_random_val true)
	   else
	     label k
	     @@ sw fp 0 sp
	     @@ sw ra (-4) sp
	     @@ label (k^"_tail_rec_begin")
	     @@ move fp sp
	     @@ subi sp sp (24+(4*(List.length (Symb_Tbl.bindings v.locals))))
	     @@ instr
	     @@ (return_random_val v.full_convention))
      | Some(x) ->
	 Some(
	   if not v.full_convention then
	     x
	     @@ label k
	     @@ sw fp 0 sp
	     @@ move fp sp
	     @@ subi sp sp (20+(4*(List.length (Symb_Tbl.bindings v.locals))))
	     @@ instr
	     @@ (return_random_val v.full_convention)
	     @@ label (k^"_tail_rec_begin")
	     @@ move fp sp
	     @@ subi sp sp (24+(4*(List.length (Symb_Tbl.bindings v.locals))))
	     @@ (fst (translate_instruction v.code 0 (List.mapi (fun i elt -> (i+1, elt)) v.signature.formals) true (List.mapi (fun i elt -> (i, elt)) (Symb_Tbl.bindings v.locals)) program.functions true))
	     @@ (return_random_val true)
	   else
	     x
	     @@ label k
	     @@ sw fp 0 sp
	     @@ sw ra (-4) sp
	     @@ label (k^"_tail_rec_begin")
	     @@ move fp sp
	     @@ subi sp sp (24+(4*(List.length (Symb_Tbl.bindings v.locals))))
	     @@ instr
	     @@ (return_random_val v.full_convention))) None fun_list in
  let program_code = match fun_code with
    | None -> main_code @@ close
    | Some(x) -> main_code @@ close @@ x
  in
  let text = init @@ program_code @@ (print_char () @@ print_string () @@ invalid_array_access_1 () @@ invalid_array_access_2 () @@ invalid_array_size () @@ print_int_err ()
					    @@ array_not_initialized () @@ integer_out_range ()) @@ built_ins in

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
