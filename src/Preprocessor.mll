{

  (* Contexte *)
  open Lexing

  module Macro = Map.Make(String)

  let macros = ref (Macro.empty)
  let name = ref ""
  let text = ref ""
  let pp_str = ref ""
  let args = ref 0
  let arg_values = ref [||]
  let arg_reading = ref ""

  (* add a macro in macros table : if the name of the macro is already in the table then we replace its value so it means that we consider at each part of the program the last value associated with macro's name *)
  let add_macro n (a,t) = macros := (Macro.add n (a,t) (!macros))
        
}

let alpha = ['a'-'z' 'A'-'Z']
let const_number = ['-']?(['1'-'9']+['0'-'9']*)+

rule define_or_replace = parse
| "#DEFINE"
    { macro_name lexbuf }
| '\n'
    { pp_str := !pp_str ^ (lexeme lexbuf); let () = new_line lexbuf in define_or_replace lexbuf }
| '#'alpha+
    {
      let macro_name = (String.sub (lexeme lexbuf) 1 ((String.length (lexeme lexbuf)) - 1)) in
      try
	let (a, t) = (Macro.find macro_name (!macros)) in
	if a = 0 then
	  begin
	    pp_str := !pp_str ^ t;
	    define_or_replace lexbuf
	  end
	else
	  begin
	    text := t;
	    arg_values := Array.make a "";
	    read_args lexbuf
	  end
      with
      | Not_found -> failwith (Printf.sprintf "macro %s not previously defined : at line %d and column %d" macro_name lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))
    }
(* comments *)
| ("//"[^'\n']*)
    { pp_str := !pp_str ^ (lexeme lexbuf); define_or_replace lexbuf }
| "/*"
    { pp_str := !pp_str ^ (lexeme lexbuf); long_comment lexbuf }
| eof
    { !pp_str }
| _
    { pp_str := !pp_str ^ (lexeme lexbuf); define_or_replace lexbuf }

and long_comment = parse
| "*/"
    { pp_str := !pp_str ^ (lexeme lexbuf); define_or_replace lexbuf }
| eof
    { !pp_str }
| '\n'
    { pp_str := !pp_str ^ (lexeme lexbuf); let () = new_line lexbuf in define_or_replace lexbuf }
| _
    { pp_str := !pp_str ^ (lexeme lexbuf); long_comment lexbuf }

and macro_name = parse
| [' ' '\t']+alpha+
    { name := String.trim (lexeme lexbuf); macro_args lexbuf }
  (* name error *)
| _ { failwith (Printf.sprintf "macro name error : %s at line %d and column %d" (lexeme lexbuf) lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)) }

and macro_args = parse
| [' ' '\t']+
    { macro_args lexbuf }
| "{"const_number"}"
    {
      args := int_of_string (String.sub (lexeme lexbuf) 1 ((String.length (lexeme lexbuf)) - 2));
      macro_text lexbuf
    }
| eof
    { failwith(Printf.sprintf "expected <text> at line %d and column %d" lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)) }
| _
    { macro_text lexbuf }
      
and macro_text = parse
| [' ' '\t']+
    { macro_text lexbuf }
| ([^' ' '\t' '\n'][^'\n']+[^' ' '\t' '\n']) | [^' ' '\t' '\n']
    { text := String.trim (lexeme lexbuf); macro_text lexbuf }
| '\n'
    { let () = new_line lexbuf in add_macro (!name) ((!args),(!text)); text := ""; name := ""; args := 0; define_or_replace lexbuf }
| eof
    { !pp_str }

and read_args = parse
| [' ' '\t']
    { read_args lexbuf }
| '{'
    { read_s_arg lexbuf }
| _
    { failwith(Printf.sprintf "found %d arguments but expected %d arguments" (!args) (Array.length (!arg_values))) }

and read_s_arg = parse
| '{' | '\n'
    { failwith (Printf.sprintf "syntax error with macro call at line %d and column %d, one '}' is missing" lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))}
| '}'
    {
      (!arg_values).(!args) <- !arg_reading;
      incr args;
      arg_reading := "";
      if !args = Array.length (!arg_values) then
	begin
	  (* parse text and replace all #const_number with the arg given as parameter and check if const_number > 0 and < number of macro's arg between here *)
	  let arg_replacing = ref false in
	  let number = ref "" in
	  for i = 0 to ((String.length (!text)) - 1) do
	    if (!arg_replacing) then
	      begin
		if int_of_char (String.get (!text) i) >= 48 && int_of_char (String.get (!text) i) <= 57 then number := (!number) ^ (Printf.sprintf "%c" (String.get (!text) i))
		else if int_of_string (!number) > Array.length (!arg_values) || int_of_string (!number) <= 0 then failwith (Printf.sprintf "invalid arg identifier in macro at line %d and column %d" lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))
		else
		  begin
		    pp_str := !pp_str ^ (!arg_values).((int_of_string (!number)) - 1) ^ (Printf.sprintf "%c" (String.get (!text) i));
		    number := "";
		    arg_replacing := false
		  end
	      end
	    else if (String.get (!text) i) = '#' then arg_replacing := true
	    else pp_str := !pp_str ^ (Printf.sprintf "%c" (String.get (!text) i))
	  done;
	  if (!arg_replacing) then
	    begin
	      if int_of_string (!number) > Array.length (!arg_values) || int_of_string (!number) <= 0 then failwith (Printf.sprintf "invalid arg identifier in macro at line %d and column %d" lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))
	      else
		begin
		  pp_str := !pp_str ^ (!arg_values).((int_of_string (!number)) - 1);
		  number := "";
		  arg_replacing := false
		end
	    end;
	  (* and here *)
	  args := 0;
	  text := "";
	  define_or_replace lexbuf
	end
      else read_args lexbuf
    }
| _
    { arg_reading := (!arg_reading) ^ (lexeme lexbuf); read_s_arg lexbuf }
