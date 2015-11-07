open Core.Std
open Parser
open Lexer
open OUnit
open Printf

let to_string = function
| FIELD_ID_NUMBER         -> sprintf "FIELD_ID_NUMBER"
| FIELD_ID_TITLE          -> sprintf "FIELD_ID_TITLE"
| FIELD_ID_VOICE          -> sprintf "FIELD_ID_VOICE"
| FIELD_ID_TEMPO          -> sprintf "FIELD_ID_TEMPO"
| FIELD_ID_KEY            -> sprintf "FIELD_ID_KEY"
| FIELD_ID_METER          -> sprintf "FIELD_ID_METER"
| FIELD_ID_DEFAULT_LENGTH -> sprintf "FIELD_ID_DEFAULT_LENGTH"
(*
| INT(d)                  -> sprintf "INT(%d)" d
| TEXT(s)                 -> sprintf "TEXT(%s)" s
*)
| DIGIT                   -> sprintf "DIGIT"
| A                       -> sprintf "A"
| B                       -> sprintf "B"
| C                       -> sprintf "C"
| D                       -> sprintf "D"
| E                       -> sprintf "E"
| F                       -> sprintf "F"
| G                       -> sprintf "G"
| M                       -> sprintf "M"
| Z                       -> sprintf "Z"
| COLON                   -> sprintf "COLON"
| CARET                   -> sprintf "CARET"
| DOUBLE_CARET            -> sprintf "DOUBLE_CARET"
| UNDERSCORE              -> sprintf "UNDERSCORE"
| DOUBLE_UNDERSCORE       -> sprintf "DOUBLE_UNDERSCORE"
| FORWARD_SLASH           -> sprintf "FORWARD_SLASH"
| SINGLE_QUOTE            -> sprintf "SINGLE_QUOTE"
| EQUAL                   -> sprintf "EQUAL"
| COMMA                   -> sprintf "COMMA"
| LEFT_BRACK              -> sprintf "LEFT_BRACK"
| RIGHT_BRACK             -> sprintf "RIGHT_BRACK"
| LEFT_PARAM              -> sprintf "ELFT_PARAM"
| BAR                     -> sprintf "BAR"
| DOUBLE_BAR              -> sprintf "DOUBLE_BAR"
| LEFT_BRACK_BAR          -> sprintf "LEFT_BRACK_BAR"
| BAR_RIGHT_BRACK         -> sprintf "BAR_RIGHT_BRACK"
| COLON_BAR               -> sprintf "COLON_BAR"
| BAR_COLON               -> sprintf "BAR_COLON"
| COMMENT                 -> sprintf "COMMENT"
| SHARP                   -> sprintf "SHARP"
| REPEAT_ONE              -> sprintf "REPEAT_ONE"
| REPEAT_TWO              -> sprintf "REPEAT_TWO"
| STRING(s)               -> sprintf "STRING(%s" s
| EOF                     -> sprintf "EOF"

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  Parser.prog Lexer.token lexbuf;
  In_channel.close inx

let () =
  Command.basic ~summary:"Parse and display ABC notation"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run
