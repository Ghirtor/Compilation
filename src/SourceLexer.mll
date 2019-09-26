{

  (* Contexte *)
  open Lexing
  open SourceParser

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
  | [' ' '\t' '\r']
      { token lexbuf }
  (* sauts de lignes ignorés *)
  | '\n' { let () = new_line lexbuf in token lexbuf (* update line number to locate easily errors *) }
  (* Les chaînes alphabétiques sont traitées par la fonction [id_or_keyword]
     pour être associées à des mots-clés ou des identifiants. *)
  | alpha+
      { id_or_keyword (lexeme lexbuf) }
  | const_number
      {CONST_INT(int_of_string (lexeme lexbuf))}
  (* Début et fin de bloc *)
  | "{" { BEGIN }
  | "}" { END }
  (* brackets *)
  | "[" { LB }
  | "]" { RB }
  (* semi *)
  | ";" { SEMI }
  (* comma *)
  | "," { COMMA }
  (* operators *)
  | ":=" { SET }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { DIV }
  | "%" { MOD }
  | "==" { EQUAL }
  | "=" { INIT }
  | "!=" { NEQ }
  | "<=" { LE }
  | "<" { LT }
  | ">=" { GE }
  | ">" { GT }
  | "&&" { AND }
  | "||" { OR }
  | "!" { NOT }
  (* lp rp *)
  | "(" { LP }
  | ")" { RP }
  (* . *)
  | "." { DOT }
  (* comments *)
  | "//"[^'\n']*eof { EOF }
  | "//"[^'\n']* { token lexbuf }
  | "/*" { long_comment lexbuf }
  (* Fin de fichier *)
  | eof
      { EOF }
  (* Caractères non reconnus *)
  | _
      { failwith (Printf.sprintf "Unknown character : %s at line %d and column %d" (lexeme lexbuf) lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)) }

and long_comment = parse
  | '\n'
      { let () = new_line lexbuf in long_comment lexbuf }
  | "*/"
      { token lexbuf }
  | _
      { long_comment lexbuf }
  | eof
      { failwith (Printf.sprintf "error : comment not terminated at line %d and column %d" lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)) }
