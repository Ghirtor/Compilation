open CommonAST
module Indexed = IndexedGotoAST

type succ_table = int list array

type liveness_info = { live_in: string list array;
                       live_out: string list array }
  
let index_of_instruction (i : IndexedGotoAST.instruction) =
  match i with
  | (n, _) -> n

let rec index_of_labels_and_size (i : IndexedGotoAST.instruction) =
  match i with
  | (_, Sequence(instr1, instr2)) ->
     let (labels1, size1) = index_of_labels_and_size instr1 in
     let (labels2, size2) = index_of_labels_and_size instr2 in
     (Symb_Tbl.union (fun v1 v2 -> failwith "same number assigned for both labels") labels1 labels2, size2)
  | (num, Label(Lab(l))) -> (Symb_Tbl.singleton l num, num+1)
  | (num, _) -> (Symb_Tbl.empty, num+1)
     
let mk_succ_pred_table (i : Indexed.instruction) =
  let (indexed_labels, size) = index_of_labels_and_size i in
  let succ_table = Array.init size (fun ind -> []) in
  let pred_table = Array.init size (fun ind -> []) in
  let rec aux (i : Indexed.instruction) =
    match i with
    | (num, Sequence(instr1, instr2)) ->
       aux instr1;
      aux instr2
    | (num, Goto(Lab(l))) ->
       pred_table.(Symb_Tbl.find l indexed_labels) <- num::(pred_table.(Symb_Tbl.find l indexed_labels));
       succ_table.(num) <- ([Symb_Tbl.find l indexed_labels])
    | (num, ConditionalGoto(Lab(l), expr)) ->
       if num+1 < size then
	 begin
	   pred_table.(num+1) <- num::(pred_table.(num+1));
	   pred_table.(Symb_Tbl.find l indexed_labels) <- num::(pred_table.(Symb_Tbl.find l indexed_labels))
	 end;
      succ_table.(num) <- (if num+1 < size then [num+1; Symb_Tbl.find l indexed_labels] else [Symb_Tbl.find l indexed_labels])
    | (num, _) ->
       if num+1 < size then pred_table.(num+1) <- num::(pred_table.(num+1));
      succ_table.(num) <- (if num+1 < size then [num+1] else [])
  in aux i; (succ_table, pred_table)

let remove_from_list l1 elt =
  match elt with
  | None -> l1
  | Some(elt) ->
     let rec aux l acc =
       match l with
       | [] -> acc
       | x::s -> if String.equal x elt then aux s acc else aux s (x::acc)
     in aux l1 []

let union_list l1 l2 =
  let rec aux l acc =
    match l with
    | [] -> acc
    | x::s ->
       try
	 let _ = List.find (fun elt -> String.equal elt x) acc in
	 aux s acc
       with
       | Not_found -> aux s (x::acc)
  in aux l1 l2

let (--) l1 elt = remove_from_list l1 elt

let (+) l1 l2 = union_list l1 l2

let rec var_liveness (e : Indexed.expression) =
  match e with
  | UnaryOp(op, e) -> var_liveness e
  | BinaryOp(op, e1, e2) -> (var_liveness e1) + (var_liveness e2)
  | NewBlock(e) -> var_liveness e
  | Location(Identifier(Id(id))) -> [id]
  | Location(BlockAccess(e1, e2)) -> (var_liveness e1) + (var_liveness e2)
  | FunCall(id, l) -> 
     List.fold_left (fun acc elt -> union_list (var_liveness elt) acc) [] l
  | _ -> []

let instr_array (i : Indexed.instruction) size =
  let array = Array.init size (fun ind -> (0, Indexed.Nop)) in
  let rec aux (i : Indexed.instruction) =
    match i with
    | (num, Sequence(instr1, instr2)) ->
       aux instr1;
      aux instr2
    | (num, _) as instr ->
       array.(num) <- instr
  in aux i; array
     
let liveness (i : Indexed.instruction) =
  let modif = ref false in
  let (succ_table, pred_table) = mk_succ_pred_table i in
  let infos =
    { live_in = Array.init (Array.length succ_table) (fun ind -> []); live_out = Array.init (Array.length succ_table) (fun ind -> []) } in
  let aux1 num kill gen =
    let succ = succ_table.(num) in
    let _out = List.fold_left (fun acc elt -> (infos.live_in.(elt)) + acc) [] succ in
    let _in = (_out -- kill) + gen in
    let old_in = infos.live_in.(num) in
    let old_out = infos.live_out.(num) in
    infos.live_in.(num) <- _in;
    infos.live_out.(num) <- _out;
    if not (!modif) && (List.length old_in = List.length _in) && (List.length old_out = List.length _out) then
      begin
	try
	  List.iter (fun elt -> let _ = List.find (fun elt' -> String.equal elt elt') old_in in ()) _in;
	  List.iter (fun elt -> let _ = List.find (fun elt' -> String.equal elt elt') old_out in ()) _out;
	  modif := false
	with
	| Not_found -> modif := true
      end
    else modif := true
  in
  let rec aux2 (i : Indexed.instruction) =
    match i with
    | (num, Return(expr)) ->
       aux1 num None (var_liveness expr)
    | (num, FunCall(id, l)) ->
       aux1 num None (var_liveness (FunCall(id, l)))
    | (num, ConditionalGoto(l, expr)) ->
       aux1 num None (var_liveness expr)
    | (num, Sequence(instr1, instr2)) ->
       aux2 instr1;
      aux2 instr2
    | (num, Set(Identifier(Id(id)), expr)) ->
       aux1 num (Some(id)) (var_liveness expr)
    | (num, _) ->
       aux1 num None []
  in

  (* ///////////////////////////////////////////// without list of tasks /////////////////////////////////////////////////////////// *)
  (*aux2 i;
  while !modif do
    modif := false;
    aux2 i
    done;*)
  (* /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// *)

  (* ///////////////////////////////////////////// with list of remaining tasks //////////////////////////////////////////////////// *)
  let size = (Array.length succ_table) in
  let tasks = ref (List.init size (fun ind -> size - 1 - ind)) in
  let instr = instr_array i size in
  let current_instr = ref (0, Indexed.Nop) in
  let tasks_size = ref size in
  (match !tasks with
  | x::s -> current_instr := instr.(x); aux2 !current_instr; if !modif then tasks := s@(pred_table.(x)) else tasks := s
  | _ -> tasks_size := !tasks_size - 1; tasks := []);
  while !tasks_size > 0 do
    modif := false;
    match !tasks with
    | x::s -> current_instr := instr.(x); aux2 !current_instr; if !modif then tasks := s@(pred_table.(x)) else tasks := s
    | _ -> tasks_size := !tasks_size - 1; tasks := []
  done;
  (* /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// *)
  infos
