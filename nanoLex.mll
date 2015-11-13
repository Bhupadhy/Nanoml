{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}
rule token = parse
    | eof         { EOF }
    | [' ' '\t' '\r' '\n'] 	{ token lexbuf }
    | "true"    		{ TRUE }
    | "false"    		{ FALSE }
    | "let"			{ LET }
    | "rec"                     { REC }
    | "in"                      { IN }
    | "=" 			{ EQ }
    | "fun"			{ FUN }
    | "->"			{ ARROW }
    | "if"			{ IF }
    | "then"			{ THEN }
    | "else"			{ ELSE }
    | "+"			{ PLUS }
    | "-"			{ MINUS }
    | "*"			{ MUL }
    | "/"			{ DIV }
    | "<="			{ LE }
    | "<"			{ LT }
    | "!="			{ NE }
    | "||"			{ OR }
    | "&&"			{ AND }
    | "("			{ LPAREN }
    | ")"			{ RPAREN }
    | "["			{ LBRAC }
    | "]"			{ RBRAC }
    | ";"			{ SEMI }
    | "::"			{ COLONCOLON }
    | ['0'-'9']* as l           { Num(int_of_string l) }
    | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']* as str       { Id(str)} 
    | _           		{ raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
