{

  (* Contexte *)
  open Lexing
  open SourceParser

  let token_queue = Queue.create ()
  let indentation_stack = ref [0]
  let blanks = ref 0
  let indenting = ref false
  let empty_main = ref true (* var to handle empty main case *)

  let pop_from_indentation_stack b lexbuf =
    let rec aux stack =
      match stack with
      | [] -> ()
      | x::s ->
	 if x > b then
	   begin
	     Queue.push END token_queue;
	     indentation_stack := s;
	     aux s
	   end
	 else if x < b then failwith (Printf.sprintf "bad indentation at line %d and column %d" lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))
    in aux (!indentation_stack)

  let close_blocks lexbuf =
    let res = match (!indentation_stack) with
      | [] -> -1
      | x::s -> x
    in
    if !blanks > res then
      begin
	Queue.push BEGIN token_queue;
	empty_main := false;
	indentation_stack := (!blanks)::(!indentation_stack)
      end
    else if !blanks < res then pop_from_indentation_stack !blanks lexbuf


  let next_token t lexbuf =
    close_blocks lexbuf;
    indenting := false; Queue.push t token_queue; Queue.pop token_queue

  (* Traitement des chaînes de caractères alphabétiques *)
  let id_or_keyword =
    (* Définition d'une table des mots-clés et des lexèmes associés *)
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [	"main", MAIN; "var", VAR; "integer", INTEGER; "boolean", BOOLEAN; "if", IF; "else", ELSE; "elif", ELIF; "while", WHILE; "for", FOR; "break", BREAK; "continue", CONTINUE; "true", CONST_BOOL(true); "false", CONST_BOOL(false); "new", NEW; "struct", STRUCT; "return", RETURN;
      ] ;
    fun s ->
      (* On cherche la chaîne [s] dans la table. Si on trouve un mot-clé alors
         on le renvoie. *)
      try  Hashtbl.find h s
      (* Et sinon on considère qu'il s'agit d'un identifiant. *)
      with Not_found -> IDENT(s)
        
}

(* Raccourci : caractères alphabétiques *)
let alpha = ['a'-'z' 'A'-'Z']
(* shortcut for numbers *)
let const_number = (['1'-'9']+['0'-'9']*)+|'0'

(* Expressions régulières définissant les lexèmes *)
rule token = parse
  (* Les espaces, tabulations, sauts de ligne sont ignorés *)
  | [' '] { if !indenting then incr blanks; token lexbuf }
  | ['\t'] { if !indenting then blanks := !blanks + 4; token lexbuf }  
  | ['\r']
      { token lexbuf }
  (* sauts de lignes ignorés *)
  | '\n'
      {
        indenting := true; blanks := 0; let () = new_line lexbuf in token lexbuf (* update line number to locate easily errors *)
      }
  (* Les chaînes alphabétiques sont traitées par la fonction [id_or_keyword]
     pour être associées à des mots-clés ou des identifiants. *)
  | alpha+
      {
	close_blocks lexbuf;
	let t = (id_or_keyword (lexeme lexbuf)) in
	match t with
	| MAIN -> indenting := false; Queue.push t token_queue; Queue.pop token_queue
	| VAR | IF | WHILE | FOR | BREAK | CONTINUE | NEW | STRUCT | RETURN -> if !indenting then Queue.push SEMI token_queue; indenting := false; Queue.push t token_queue; Queue.pop token_queue
	| _ -> indenting := false; Queue.push t token_queue; Queue.pop token_queue
      }
  | alpha+ [' ' '\t']* ":="
      {
	close_blocks lexbuf;
        if !indenting then Queue.push SEMI token_queue; indenting := false; Queue.push (id_or_keyword (String.trim (String.sub (lexeme lexbuf) 0 (String.length (lexeme lexbuf) - 2)))) token_queue; Queue.push SET token_queue; Queue.pop token_queue
      }
  | const_number
      { next_token (CONST_INT(int_of_string (lexeme lexbuf))) lexbuf }
  (* comma *)
  | "," { next_token COMMA lexbuf }
  (* operators *)
  | ":=" { next_token SET lexbuf }
  | "=" { next_token INIT lexbuf }
  | "+" { next_token PLUS lexbuf }
  | "-" { next_token MINUS lexbuf }
  | "*" { next_token STAR lexbuf }
  | "/" { next_token DIV lexbuf }
  | "%" { next_token MOD lexbuf }
  | "==" { next_token EQUAL lexbuf }
  | "!=" { next_token NEQ lexbuf }
  | "<=" { next_token LE lexbuf }
  | "<" { next_token LT lexbuf }
  | ">=" { next_token GE lexbuf }
  | ">" { next_token GT lexbuf }
  | "&&" { next_token AND lexbuf }
  | "||" { next_token OR lexbuf }
  | "!" { next_token NOT lexbuf }
  (* brackets *)
  | "[" { next_token LB lexbuf }
  | "]" { next_token RB lexbuf }
  (* . *)
  | "." { next_token DOT lexbuf }
  (* lp rp *)
  | "(" { next_token LP lexbuf }
  | ")" { next_token RP lexbuf }
  (* comments *)
  | "//"[^'\n']*eof
      {
	if !empty_main then (* case to handle empty main *)
	  begin
	    Queue.push BEGIN token_queue;
	    Queue.push END token_queue;
	  end;
	blanks := 0;
	close_blocks lexbuf;
	Queue.push EOF token_queue;
	try
	  Queue.pop token_queue
	with
	| Queue.Empty -> EOF
      }
  | "//"[^'\n']* { blanks := 0; indenting := false; token lexbuf }
  | "/*" { if !indenting then blanks := !blanks + 2; long_comment lexbuf }
  (* Fin de fichier *)
  | eof
      {
	if !empty_main then (* case to handle empty main *)
	  begin
	    Queue.push BEGIN token_queue;
	    Queue.push END token_queue;
	  end;
	blanks := 0;
	close_blocks lexbuf;
	Queue.push EOF token_queue;
	try
	  Queue.pop token_queue
	with
	| Queue.Empty -> EOF
      }
  (* Caractères non reconnus *)
  | _
      { failwith (Printf.sprintf "Unknown character : %s at line %d and column %d" (lexeme lexbuf) lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)) }

and long_comment = parse
  | '\n'
      { blanks := 0; let () = new_line lexbuf in long_comment lexbuf }
  | "*/"
      { if !blanks != -1 then blanks := !blanks + 2; token lexbuf }
  | _
      { if !blanks != -1 then incr blanks; long_comment lexbuf }
  | eof
      { failwith (Printf.sprintf "error : comment not terminated at line %d and column %d" lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)) }
