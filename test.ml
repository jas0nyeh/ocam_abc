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
(*
| FIELD_ID_COMPOSER       -> sprintf "FIELD_ID_COMPOSER"
| FIELD_ID_DEFAULT_LENGTH -> sprintf "FIELD_ID_DEFAULT_LENGTH"
| FIELD_ID_KEY            -> sprintf "FIELD_ID_KEY"
*)
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
| LEFT_PARAM              -> sprintf "LEFT_PARAM"
| BAR                     -> sprintf "BAR"
| DOUBLE_BAR              -> sprintf "DOUBLE_BAR"
| LEFT_BRACK_BAR          -> sprintf "LEFT_BRACK_BAR"
| BAR_RIGHT_BRACK         -> sprintf "BAR_RIGHT_BRACK"
| COLON_BAR               -> sprintf "COLON_BAR"
| BAR_COLON               -> sprintf "BAR_COLON"
| SHARP                   -> sprintf "SHARP"
| COMMENT                 -> sprintf "COMMENT"
| REPEAT_ONE              -> sprintf "REPEAT_ONE"
| REPEAT_TWO              -> sprintf "REPEAT_TWO"
| STRING(s)               -> sprintf "STRING(%s)" s
(*
| SHARP                   -> sprintf "SHARP"
| FLAT                    -> sprintf "FLAT"
| NEUTRAL                 -> sprintf "NEUTRAL"
| SINGLE_QUOTE            -> sprintf "SINGLE_QUOTE"
| BAR                     -> sprintf "BAR"
| LEFT_BRACK              -> sprintf "LEFT_BRACK"
| RIGHT_BRACK             -> sprintf "RIGHT_BRACK"
*)

(*
| FIELD_NUMBER(_)   -> sprintf "FIELD_NUMBER"
| FIELD_TITLE(_)    -> sprintf "FIELD_TITLE"
| FIELD_COMPOSER(s) -> sprintf "FIELD_COMPOSER(%s)" s
| FIELD_METER(s)    -> sprintf "FIELD METER(%s)" s
| METER(s)          -> sprintf "Meter(%s)" s
| FIELD_KEY(s)      -> sprintf "FIELD_KEY(%s)" s
| FIELD_TEMPO(s)    -> sprintf "FIELD_TEMPO(%s)" s
*)
| EOF               -> sprintf "EOF"

let do_lex x =
  (* let lexbuf = set_filename "stdin" @@ Lexing.from_channel stdin in *)
  (* let lexbuf = set_filename "stdin" @@ Lexing.from_string "123\n456" in *)
  let lexbuf = set_filename "stdin" @@ Lexing.from_string x in
  let rec loop acc = function
    | EOF -> EOF::acc |> List.rev
    | x   -> loop (x :: acc) (token lexbuf) in
  loop [] (token lexbuf)

let print_failure expected real =
  printf "\n--- Failure Message ---\n";
  printf "-- Expected --\n";
  List.iter ~f:(fun y -> printf "%s\n" @@ to_string y) expected;
  printf "-- Real --\n";
  List.iter ~f:(fun y -> printf "%s\n" @@ to_string y) real;
  printf "--- ---\n"

let do_test expected real =
  try
    assert_equal expected real;
  with
    | e -> print_failure expected real; raise e

let test_l_field_number _ =
  do_test [FIELD_ID_NUMBER;DIGIT;EOF] @@ do_lex "X: 15\n"

let test_field_title _ =
  do_test [FIELD_ID_TITLE;STRING("Valse Romantique");EOF] @@ do_lex "T: Valse Romantique\n"

let test_field_header _ =
  do_test [FIELD_ID_NUMBER;DIGIT;FIELD_ID_TITLE;STRING("Valse Romantique");FIELD_ID_KEY;STRING("D");EOF]
    @@ do_lex ("X: 15\n" ^ "T: Valse Romantique\n" ^ "K:D\n")


let test_l_notes _ =
  do_test [A;B;C;EOF] @@ do_lex "A B C\n";
  do_test [A;Z;C;EOF] @@ do_lex "A z C\n"

let test_l_accidental _ =
  do_test [DOUBLE_CARET;CARET;EQUAL;UNDERSCORE;DOUBLE_UNDERSCORE;EOF]
          @@ do_lex "^^ ^ = _ __\n"

let test_l_note_length _ =
  do_test [DIGIT;FORWARD_SLASH;DIGIT;EOF] @@ do_lex "10/5\n"

let test_octave _ =
  do_test [COMMA;SINGLE_QUOTE;COMMA;EOF] @@ do_lex ", \' ,\n"

let test_l_header_music  _ =
  let s = "X: 15\nT: Valse Romantique\nA a B b\n" in
  do_test (FIELD_ID_NUMBER::STRING(" 15")::
           FIELD_ID_TITLE::STRING(" Valse Romantique")::
           A::A::B::B::EOF::[])
           @@ do_lex s

let do_parse x =
  let lexbuf = set_filename "stdin" @@ Lexing.from_string x in
  Parser.prog Lexer.token lexbuf
  (* assert_equal (Some (`String "A Header")) (Parser.prog Lexer.token lexbuf) *)

let header = "X: 15\nT: Valse Romantique\nK:D\n"

(*
let test_temp_parse _ =
  do_parse @@ header ^ "C2,\n"
*) 

let test_parse_notes_rest _ =
  let s = header ^ "A\n" in
  do_test (FIELD_ID_NUMBER::DIGIT::
           FIELD_ID_TITLE::STRING("Valse Romantique")::
           FIELD_ID_KEY::STRING("D")::
           A::EOF::[])
          @@ do_lex s;
  do_parse s;
  do_parse @@ header ^ "B\n";
  do_parse @@ header ^ "z\n";
  let s = header ^ "Az\n" in
  do_test (FIELD_ID_NUMBER::DIGIT::
           FIELD_ID_TITLE::STRING("Valse Romantique")::
           FIELD_ID_KEY::STRING("D")::
           A::Z::EOF::[])
          @@ do_lex s;
  do_parse s;
  do_parse @@ header ^ "AzB\n";
  do_parse @@ header ^ "ABz\n";
  do_parse @@ header ^ "zzABz\n";
  do_parse @@ header ^ "zzzA\n"

let test_parse_notes_rest_length _ =
  do_parse @@ header ^ "A10z200\n";
  do_parse @@ header ^ "Az200B\n";
  do_parse @@ header ^ "Az5BBz5\n";
  do_parse @@ header ^ "z200B5A87\n";
  do_parse @@ header ^ "z/200B5A87\n";
  do_parse @@ header ^ "z50/200B5A87\n"

let test_parse_accidental _ =
  do_parse @@ header ^ "^^A\n";
  do_parse @@ header ^ "z^^A=B=Bzzz\n";
  do_parse @@ header ^ "^B\n";
  do_parse @@ header ^ "^BBB_A\n";
  do_parse @@ header ^ "^B__BBAA_A\n"

let test_parse_octave _ =
  do_parse @@ header ^ "BA\'\'";
  do_parse @@ header ^ "A\'";
  do_parse @@ header ^ "zB,";
  do_parse @@ header ^ "B,,A'''z"

let test_parse_pitch _ =
  do_parse @@ header ^ "zzz__AB10z/1z10/50B,,"

let test_multi_note _ =
  do_parse @@ header ^ "[ABzA]%THIS is comment\n";
  do_parse @@ header ^ "[ABzA][ABz]\n";
  do_parse @@ header ^ "[zzz__AB10z/1z10/50B,,]\n";
  do_parse @@ header ^ "[^^B,,A''10/50z30\n]"

let test_multi_tuplet _ =
  do_parse @@ header ^ "(5ABz\n";
  do_parse @@ header ^ "(5ABz(3AAA10z(2^^B,,A''10\n";
  do_parse @@ header ^ "zA(5ABz\n"

let test_lex_temp _ =
  do_test [A;B;C;EOF] @@ do_lex "z3 | z3 | z3 | z3 | z/2 a/2 a a | z/2 b/2 b a' |\n"

let test_field_voice _ =
  (*
  let s = header ^ "% m1-6\n" ^ "V: 1\n" ^ "z3 | z3 | z3 | z3 | z/2 g/2 g a | z/2 b/2 b c' |\n" in
  do_test (FIELD_ID_NUMBER::STRING(" 15")::
           FIELD_ID_TITLE::STRING(" Valse Romantique")::
           A::EOF::[])
          @@ do_lex s
  *)
  let s = header ^ "% m1-6\n" ^ "V: 1\n" ^ "z3 | z3 | z3 | z3 | z/2 g/2 g a | z/2 b/2 b c' |\n" in
  do_parse s

let test_prelude _ =
  let s = "X:8628\n" ^ "T:Prelude BWV 846 no. 1\n" ^ "C:Johann Sebastian Bach\n" ^ "K:C\n" in
  do_test  (FIELD_ID_NUMBER::DIGIT::
            FIELD_ID_TITLE::STRING("Prelude BWV 846 no. 1")::
            C::STRING("Johann Sebastian Bach")::
            FIELD_ID_KEY::STRING("C")::EOF::[])
           @@ do_lex s

let suite = "OUnit Example" >:::
  [
   "test_field_number"    >:: test_l_field_number;
   "test_field_title"     >:: test_field_title;
   "test_field_header"     >:: test_field_header;
   (*
   "test_notes"           >:: test_l_notes;
   "test_l_accidental"    >:: test_l_accidental;
   "test_l_note_length"   >:: test_l_note_length;
   "test_octave"          >:: test_octave;
   "test_l_header_music"  >:: test_l_header_music;
   *)
   "test_parse_notes_rest" >:: test_parse_notes_rest;
   (*
   "test_parse_notes_rest_length" >:: test_parse_notes_rest_length;
   "test_parse_notes_rest_length" >:: test_parse_notes_rest_length;
   "test_parse_octave" >:: test_parse_octave;
   "test_parse_accidental" >:: test_parse_accidental;
   "test_parse_pitch" >:: test_parse_pitch;
   "test_multi_note" >:: test_multi_note;
   "test_multi_tuplet" >:: test_multi_tuplet;
   "test_field_voice" >:: test_field_voice;
   *)
   (*"test_temp_parse" >:: test_temp_parse; *)
   "test_prelude" >:: test_prelude;
  ]

let _ = run_test_tt_main suite
