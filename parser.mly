%token DIGIT
%token <string> STRING

%token A
%token B
%token C
%token D
%token E
%token F
%token G
%token M
%token Z
%token FORWARD_SLASH
%token EOF
%token CARET
%token DOUBLE_CARET
%token UNDERSCORE
%token DOUBLE_UNDERSCORE
%token EQUAL
%token SINGLE_QUOTE
%token COMMA
%token COLON
%token RIGHT_BRACK
%token LEFT_BRACK
%token LEFT_PARAM
%token BAR
%token DOUBLE_BAR
%token LEFT_BRACK_BAR
%token BAR_RIGHT_BRACK
%token COLON_BAR
%token BAR_COLON
%token SHARP
%token REPEAT_ONE
%token REPEAT_TWO

%token FIELD_ID_NUMBER
%token FIELD_ID_TITLE
%token FIELD_ID_VOICE
%token FIELD_ID_TEMPO
%token FIELD_ID_KEY
%token FIELD_ID_METER
%token FIELD_ID_DEFAULT_LENGTH

%token COMMENT

%start <unit> prog
%%

prog: header music EOF { }

header:
  FIELD_ID_NUMBER STRING
  FIELD_ID_TITLE STRING
  option(composer)
  option(meter)
  option(default_length)
  option(field_tempo)
  list(field_voice)
  field_key
  { };

default_length:
  FIELD_ID_DEFAULT_LENGTH STRING {};

composer:
  C STRING {};

meter:
  FIELD_ID_METER STRING {};

music:
  | line+ { };

line:
  | element { }
  | field_voice { }
  | field_tempo { }
  | field_key { };

element:
  | tuplet_element {}
  | note_element {}
  | barline {}
  | nth_repeat {};

nth_repeat:
  | REPEAT_ONE {}
  | REPEAT_TWO {};

field_voice:
  | FIELD_ID_VOICE STRING {};

field_tempo:
  | FIELD_ID_TEMPO STRING {};

field_key:
  | FIELD_ID_KEY STRING {};
(*  | FIELD_ID_KEY COLON base_note mode_minor? {}; *)
(*  | FIELD_ID_KEY COLON base_note key_accidental* mode_minor? {}; *)

key_accidental:
  | SHARP {}
  | B {}

mode_minor:
  | M {}

tuplet_element:
  | LEFT_PARAM DIGIT note_element {};

note_element:
  | multi_note {}
  | note_or_rest {};

barline:
  | BAR {}
  | DOUBLE_BAR {}
  | LEFT_BRACK_BAR {}
  | BAR_RIGHT_BRACK {}
  | COLON_BAR {}
  | BAR_COLON {};


multi_note:
  | LEFT_BRACK note_or_rest_2+ RIGHT_BRACK {};

note_or_rest_2:
  | pitch note_length? {}
  | rest note_length?  {};

note_or_rest:
  | pitch note_length?  {}
  | rest note_length?  {};

rest:
  | Z {};

pitch:
  | accidental? base_note octave? { };

base_note:
  | A {  }
  | B {  }
  | C { }
  | D { }
  | E { }
  | F { }
  | G { };

accidental:
  | CARET             { }
  | DOUBLE_CARET      { }
  | UNDERSCORE        { }
  | DOUBLE_UNDERSCORE { }
  | EQUAL             { };

octave:
  | SINGLE_QUOTE+     { }
  | COMMA+            { };

note_length:
  | FORWARD_SLASH {}
  | DIGIT {}
  | DIGIT FORWARD_SLASH {}
  | DIGIT FORWARD_SLASH DIGIT {}
  | FORWARD_SLASH DIGIT {};

