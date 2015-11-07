{
open Lexing
open Parser

  (*
type token =
  | INT of int
  | FIELD_NUMBER of string
  | FIELD_TITLE of string
  | FIELD_COMPOSER of string
  | FIELD_METER of string
  | METER of string
  | FIELD_KEY of string
  | EOF (* end of input *)
  *)

let set_filename (fname:string) (lexbuf:Lexing.lexbuf)  =
  ( lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname };
     lexbuf
  )

let sprintf = Printf.sprintf

let position lexbuf =
    let p = lexbuf.Lexing.lex_curr_p in
        sprintf "%s:%d:%d" 
        p.Lexing.pos_fname p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol)

exception Error of string
let error lexbuf fmt = 
    Printf.kprintf (fun msg -> 
        raise (Error ((position lexbuf)^" "^msg))) fmt

}

(* new stuff *)
let field_id = ['X' 'T' 'C' 'L' 'M' 'Q' 'V' 'K']

(* old stuff *)
let ws    = [' ' '\t']
let nl    = ['\n' '\r']

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let basenote = ['a'-'g' 'A'-'G']
let key_accidental = '#' | 'b'

let field_number = "X:" ws* digit+ ws*
let meter =  'C' | "C|" | digit+ "/" digit+
let field_meter = "M:" ws* meter ws*
let field_key = "K:" ws* basenote key_accidental* "m"* ws*
let field_tempo = "Q:" ws* digit+ ws*

(*
let rest = 'z'
let pitch = basenote 
*)
let octave = ("\'"+) | (","+)

rule token =
  parse
(* Header *)
  | 'X'            { FIELD_ID_NUMBER }
  | 'T'            { FIELD_ID_TITLE }
  | 'V'            { FIELD_ID_VOICE }
  | 'Q'            { FIELD_ID_TEMPO }
  | 'K'            { FIELD_ID_KEY }
  | 'M'            { FIELD_ID_METER }
  | 'L'            { FIELD_ID_DEFAULT_LENGTH }
  | ':'            { read_field (Buffer.create 256) lexbuf}
(* base note *)
  | 'A'            { A }
  | 'a'            { A }
  | 'B'            { B }
  | 'b'            { B }
  | 'C'            { C }
  | 'c'            { C }
  | 'D'            { D }
  | 'd'            { D }
  | 'E'            { E }
  | 'e'            { E }
  | 'F'            { F }
  | 'f'            { F }
  | 'G'            { G }
  | 'g'            { G }
(* rest *)
  | 'z'            { Z }
(* accidental *)
  | '^'            { CARET }
  | "^^"           { DOUBLE_CARET }
  | '_'            { UNDERSCORE }
  | "__"           { DOUBLE_UNDERSCORE }
  | '='            { EQUAL }
(* octave *)
  | '\''           { SINGLE_QUOTE }
  | ','            { COMMA }
(* length *)
  | '/'            { FORWARD_SLASH }
(* multi-note *)
  | '['            { LEFT_BRACK }
  | ']'            { RIGHT_BRACK }
(* tuplet *)
  | '('            { LEFT_PARAM }
(* bar line *)
  | '|'            { BAR }
  | "||"           { DOUBLE_BAR }
  | "[|"           { LEFT_BRACK_BAR }
  | "|]"           { BAR_RIGHT_BRACK }
  | ":|"           { COLON_BAR }
  | "|:"           { BAR_COLON }
  | ':'            { COLON }
  | digit+         { DIGIT  }
  | nl             { Lexing.new_line lexbuf; token lexbuf }
  | ws+            { token lexbuf }
(* mode minor *)
  | 'm'            { M }
(* key accidental *)
(*  | '#'            { SHARP } *)
(* repeat *)
  | "[1"           { REPEAT_ONE }
  | "[2"           { REPEAT_TWO }
(* comment *)
  | '%'      { read_comment (Buffer.create 256) lexbuf }
(*
  | meter          { METER(Lexing.lexeme lexbuf) }
*)
  (*
  | field_number   { FIELD_NUMBER(Lexing.lexeme lexbuf) }
  | field_title    { FIELD_TITLE(Lexing.lexeme lexbuf) }
  | field_composer { FIELD_COMPOSER(Lexing.lexeme lexbuf) }
  | field_meter    { FIELD_METER(Lexing.lexeme lexbuf) }
  | meter          { METER(Lexing.lexeme lexbuf) }
  | field_key      { FIELD_KEY(Lexing.lexeme lexbuf) }
  | basenote       { FIELD_KEY(Lexing.lexeme lexbuf) }
  | key_accidental { FIELD_KEY(Lexing.lexeme lexbuf) }
  | field_tempo    { FIELD_TEMPO(Lexing.lexeme lexbuf) }
  *)
  | eof            { EOF }
  | _              { error lexbuf "found '%s' - don't know how to handle" @@ Lexing.lexeme lexbuf }
and read_comment buf =
  parse
  | nl             { Lexing.new_line lexbuf; token lexbuf }
  | [^ '\r' '\n']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_comment buf lexbuf
    }
and read_field buf =
  parse
  | nl             { 
                    (* Printf.printf "JYY nl"; *)
                    STRING (Buffer.contents buf) }
  | ws+            { (* Printf.printf "JYY ws+"; *)
                     read_field buf lexbuf }
  | [^' ' '\r' '\n'][^ '\r' '\n']*
    { let temp = Lexing.lexeme lexbuf in
      (* Buffer.add_string buf (Lexing.lexeme lexbuf); *)
      (* Printf.printf "JYY temp = %s\n" temp; *)
      Buffer.add_string buf temp;
      read_field buf lexbuf
    }
