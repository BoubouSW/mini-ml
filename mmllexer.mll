{

  open Lexing
  open Mmlparser

  exception Lexing_error of string

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "fun", FUN;
        "let", LET;
        "rec", REC;
        "in", IN;
        "if", IF;
        "then", THEN;
        "else", ELSE;
        "mod", MOD;
        "not", NEG;
        "int", INTEGER;
        "bool", BOOLEAN;
        "unit", UNIT;
        "type", TYPE;
        "mutable", MUTABLE;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "(*" 
      { comment lexbuf; token lexbuf }
  | number as n
      { CST(int_of_string n) }
  | "true"
      { BOOL(true) }
  | "false"
      { BOOL(false) }
  | ident as id
      { keyword_or_ident id }
  | "("
      { PAR_O }
  | ")"
      { PAR_F }
  | "+"
      { PLUS }
  | "*"
      { STAR }
  | "-"
      { MINUS }
  | "/"
      { SLASH }
  | "=="
      { EGAL }
  | "!="
      { NEGAL }
  | "<"
      { INFSTR }
  | "<="
      { INFEGAL }
  | "&&"
      { ESPESP }
  | "||"
      { BARBAR }
  | "->"
      { ARROW }
  | ":"
      { DPOINTS }
  | "="
      { AFF }
  | ";"
      { PV }
  | "{"
      { ACC_O }
  | "}"
      { ACC_F }
  | "."
      { POINT }
  | "<-"
      { RARROW }
  | ">"
      { SUPSTR }
  | ">="
      { SUPEGAL }
  | eof
      { EOF }
  | _
      { raise (Lexing_error ("unknown character : " ^ (lexeme lexbuf))) }

and comment = parse
  | "*)"
      { () }
  | "(*"
      { comment lexbuf; comment lexbuf }
  | _
      { comment lexbuf }
  | eof
      { raise (Lexing_error "unterminated comment") }
