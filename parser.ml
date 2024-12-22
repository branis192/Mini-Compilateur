type token =
  | EOF
  | DIMENSIONS
  | END
  | OF
  | ASSIGN
  | COMMA
  | LBRACKET
  | RBRACKET
  | LPAR
  | RPAR
  | DOT_DOT
  | DOT
  | PLUS
  | MOINS
  | MULT
  | MOD
  | DIV
  | NON
  | IF
  | THEN
  | ELSE
  | ELSIF
  | GT
  | GE
  | LT
  | LE
  | EQ
  | NE
  | OR
  | NOT
  | AND
  | ID of (string)
  | INT of (int)

open Parsing;;
let _ = parse_error;;
# 17 "parser.mly"

open Common
open Ast
open Printf
open Symbols

(** Raise a syntax error with the given message.
	@param msg	Message of the error. *)
let error msg =
	raise (SyntaxError msg)


(** Restructure the when assignment into selections.
	@param f	Function to build the assignment.
	@param ws	Sequence of (expression, conditions) terminated
				by (expression, NO_COND).
	@return		Built statement. *)
let rec make_when f ws =
	match ws with
	| [(e, NO_COND)]	->	f e
	| (e, c)::t			-> IF_THEN(c, f e, make_when f t)
	| _ -> failwith "whens list not ended by (expression, NO_COND)."

# 63 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* DIMENSIONS *);
  258 (* END *);
  259 (* OF *);
  260 (* ASSIGN *);
  261 (* COMMA *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* LPAR *);
  265 (* RPAR *);
  266 (* DOT_DOT *);
  267 (* DOT *);
  268 (* PLUS *);
  269 (* MOINS *);
  270 (* MULT *);
  271 (* MOD *);
  272 (* DIV *);
  273 (* NON *);
  274 (* IF *);
  275 (* THEN *);
  276 (* ELSE *);
  277 (* ELSIF *);
  278 (* GT *);
  279 (* GE *);
  280 (* LT *);
  281 (* LE *);
  282 (* EQ *);
  283 (* NE *);
  284 (* OR *);
  285 (* NOT *);
  286 (* AND *);
    0|]

let yytransl_block = [|
  287 (* ID *);
  288 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\006\000\006\000\
\007\000\007\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\003\000\003\000\010\000\010\000\010\000\010\000\
\010\000\011\000\009\000\009\000\009\000\012\000\012\000\012\000\
\012\000\013\000\013\000\013\000\013\000\013\000\013\000\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\001\000\003\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\003\000\001\000\002\000\003\000\003\000\005\000\007\000\
\009\000\005\000\001\000\003\000\003\000\001\000\003\000\003\000\
\003\000\001\000\001\000\001\000\003\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\040\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\019\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\036\000\035\000\
\000\000\007\000\000\000\000\000\034\000\000\000\030\000\000\000\
\001\000\020\000\000\000\006\000\000\000\000\000\000\000\000\000\
\038\000\039\000\017\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\037\000\000\000\000\000\
\008\000\010\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\031\000\033\000\032\000\026\000\023\000\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\025\000"

let yydgoto = "\002\000\
\004\000\009\000\021\000\010\000\011\000\033\000\034\000\035\000\
\036\000\022\000\037\000\038\000\039\000"

let yysindex = "\013\000\
\240\254\000\000\025\255\000\000\027\255\053\255\061\255\026\255\
\039\255\051\255\000\000\035\255\037\255\002\255\041\255\067\255\
\000\000\063\255\031\255\094\255\081\000\000\000\110\255\000\000\
\075\255\111\255\031\255\019\255\019\255\031\255\000\000\000\000\
\033\255\000\000\095\255\132\255\000\000\078\255\000\000\019\255\
\000\000\000\000\019\255\000\000\092\255\045\255\126\255\019\255\
\000\000\000\000\000\000\002\255\031\255\031\255\019\255\019\255\
\019\255\019\255\019\255\019\255\019\255\019\255\019\255\019\255\
\019\255\077\255\077\255\119\255\000\000\000\000\006\255\003\255\
\000\000\000\000\078\255\078\255\077\255\077\255\077\255\077\255\
\077\255\077\255\000\000\000\000\000\000\000\000\000\000\002\255\
\031\255\004\255\038\255\000\000\002\255\011\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\134\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\040\255\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\082\000\087\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\028\000\055\000\069\255\087\255\099\255\100\255\
\103\255\104\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\208\255\000\000\122\000\232\255\057\000\110\000\
\043\000\237\255\242\255\065\000\239\255"

let yytablesize = 374
let yytable = "\023\000\
\027\000\042\000\046\000\072\000\087\000\092\000\023\000\018\000\
\018\000\018\000\049\000\050\000\095\000\001\000\070\000\003\000\
\018\000\055\000\056\000\019\000\019\000\019\000\088\000\089\000\
\018\000\005\000\048\000\028\000\019\000\006\000\028\000\029\000\
\020\000\020\000\020\000\013\000\018\000\023\000\027\000\090\000\
\014\000\020\000\028\000\029\000\094\000\083\000\084\000\085\000\
\009\000\031\000\032\000\052\000\042\000\069\000\029\000\015\000\
\093\000\023\000\009\000\030\000\053\000\031\000\032\000\012\000\
\091\000\053\000\016\000\009\000\017\000\047\000\042\000\007\000\
\053\000\023\000\042\000\023\000\025\000\013\000\023\000\023\000\
\041\000\022\000\066\000\007\000\008\000\067\000\021\000\013\000\
\055\000\056\000\071\000\063\000\064\000\065\000\026\000\012\000\
\013\000\040\000\013\000\077\000\078\000\079\000\080\000\081\000\
\082\000\012\000\044\000\014\000\015\000\073\000\074\000\011\000\
\016\000\043\000\012\000\045\000\012\000\014\000\015\000\075\000\
\076\000\011\000\016\000\068\000\054\000\086\000\014\000\015\000\
\014\000\015\000\011\000\016\000\011\000\016\000\070\000\003\000\
\024\000\055\000\056\000\051\000\000\000\000\000\000\000\055\000\
\056\000\000\000\000\000\057\000\058\000\059\000\060\000\061\000\
\062\000\057\000\058\000\059\000\060\000\061\000\062\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\000\000\000\000\000\000\027\000\000\000\
\000\000\027\000\000\000\000\000\027\000\027\000\000\000\000\000\
\000\000\000\000\027\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\028\000\027\000\027\000\
\000\000\028\000\000\000\000\000\028\000\000\000\000\000\028\000\
\028\000\000\000\000\000\000\000\000\000\028\000\028\000\028\000\
\028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
\029\000\028\000\028\000\000\000\029\000\000\000\000\000\029\000\
\000\000\000\000\029\000\029\000\000\000\000\000\000\000\000\000\
\029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
\029\000\029\000\029\000\022\000\029\000\029\000\018\000\022\000\
\021\000\000\000\000\000\000\000\021\000\000\000\000\000\000\000\
\000\000\000\000\019\000\022\000\000\000\022\000\022\000\000\000\
\021\000\000\000\021\000\021\000\000\000\000\000\000\000\020\000\
\022\000\000\000\000\000\000\000\000\000\021\000"

let yycheck = "\014\000\
\000\000\021\000\027\000\052\000\002\001\002\001\021\000\006\001\
\006\001\006\001\028\000\029\000\002\001\001\000\009\001\032\001\
\006\001\012\001\013\001\018\001\018\001\018\001\020\001\021\001\
\006\001\001\001\008\001\000\000\018\001\003\001\012\001\013\001\
\031\001\031\001\031\001\010\001\006\001\052\000\008\001\088\000\
\002\001\031\001\012\001\013\001\093\000\063\000\064\000\065\000\
\009\001\031\001\032\001\019\001\072\000\009\001\000\000\005\001\
\019\001\072\000\019\001\029\001\028\001\031\001\032\001\003\001\
\089\000\028\001\032\001\028\001\032\001\027\000\090\000\031\001\
\028\001\088\000\094\000\090\000\010\001\009\001\093\000\094\000\
\000\000\000\000\040\000\031\001\032\001\043\000\000\000\019\001\
\012\001\013\001\048\000\014\001\015\001\016\001\032\001\009\001\
\028\001\004\001\030\001\057\000\058\000\059\000\060\000\061\000\
\062\000\019\001\032\001\009\001\009\001\053\000\054\000\009\001\
\009\001\004\001\028\001\005\001\030\001\019\001\019\001\055\000\
\056\000\019\001\019\001\032\001\030\001\007\001\028\001\028\001\
\030\001\030\001\028\001\028\001\030\001\030\001\009\001\002\001\
\015\000\012\001\013\001\030\000\255\255\255\255\255\255\012\001\
\013\001\255\255\255\255\022\001\023\001\024\001\025\001\026\001\
\027\001\022\001\023\001\024\001\025\001\026\001\027\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\255\255\006\001\255\255\
\255\255\009\001\255\255\255\255\012\001\013\001\255\255\255\255\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\002\001\030\001\031\001\
\255\255\006\001\255\255\255\255\009\001\255\255\255\255\012\001\
\013\001\255\255\255\255\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\002\001\030\001\031\001\255\255\006\001\255\255\255\255\009\001\
\255\255\255\255\012\001\013\001\255\255\255\255\255\255\255\255\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\002\001\030\001\031\001\006\001\006\001\
\002\001\255\255\255\255\255\255\006\001\255\255\255\255\255\255\
\255\255\255\255\018\001\018\001\255\255\020\001\021\001\255\255\
\018\001\255\255\020\001\021\001\255\255\255\255\255\255\031\001\
\031\001\255\255\255\255\255\255\255\255\031\001"

let yynames_const = "\
  EOF\000\
  DIMENSIONS\000\
  END\000\
  OF\000\
  ASSIGN\000\
  COMMA\000\
  LBRACKET\000\
  RBRACKET\000\
  LPAR\000\
  RPAR\000\
  DOT_DOT\000\
  DOT\000\
  PLUS\000\
  MOINS\000\
  MULT\000\
  MOD\000\
  DIV\000\
  NON\000\
  IF\000\
  THEN\000\
  ELSE\000\
  ELSIF\000\
  GT\000\
  GE\000\
  LT\000\
  LE\000\
  EQ\000\
  NE\000\
  OR\000\
  NOT\000\
  AND\000\
  "

let yynames_block = "\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'config) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 87 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 317 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 95 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 328 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 100 "parser.mly"
  ( set_fields _1 )
# 335 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 105 "parser.mly"
  ( [_1] )
# 342 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 108 "parser.mly"
  (_3 :: _1 )
# 350 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 113 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 362 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aux_cond1) in
    Obj.repr(
# 121 "parser.mly"
  (	_1	)
# 369 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aux_cond1) in
    Obj.repr(
# 123 "parser.mly"
  (	OR( _1,_3 )	)
# 377 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aux_cond) in
    Obj.repr(
# 127 "parser.mly"
  (	_1	)
# 384 "parser.ml"
               : 'aux_cond1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aux_cond) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aux_cond1) in
    Obj.repr(
# 129 "parser.mly"
  (	AND( _1,_3 )	)
# 392 "parser.ml"
               : 'aux_cond1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 133 "parser.mly"
 (	COMP(COMP_EQ,_1,_3)	)
# 400 "parser.ml"
               : 'aux_cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 135 "parser.mly"
 (	COMP(COMP_GE,_1,_3)	)
# 408 "parser.ml"
               : 'aux_cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 137 "parser.mly"
 (	COMP(COMP_GT,_1,_3)	)
# 416 "parser.ml"
               : 'aux_cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 139 "parser.mly"
 (	COMP(COMP_LT,_1,_3)	)
# 424 "parser.ml"
               : 'aux_cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 141 "parser.mly"
 (	COMP(COMP_LT,_1,_3)	)
# 432 "parser.ml"
               : 'aux_cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 143 "parser.mly"
 (	COMP(COMP_NE,_1,_3)	)
# 440 "parser.ml"
               : 'aux_cond))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aux_cond) in
    Obj.repr(
# 145 "parser.mly"
 (	NOT _2 	)
# 447 "parser.ml"
               : 'aux_cond))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cond) in
    Obj.repr(
# 147 "parser.mly"
 (	_2	)
# 454 "parser.ml"
               : 'aux_cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 153 "parser.mly"
  ( _1 )
# 461 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 155 "parser.mly"
  ( SEQ(_1, _2) )
# 469 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 161 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 481 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 167 "parser.mly"
  ( 
			if (get_var _1) = -1 then SET_VAR ((declare_var _1),_3) 
			else SET_VAR ((get_var _1),_3)		
		)
# 492 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 172 "parser.mly"
  (
			IF_THEN(_2,_4,NOP)
		)
# 502 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'opt_statements) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 176 "parser.mly"
  (
			IF_THEN(_2,_4,_6)
		)
# 513 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'opt_statements) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'cond) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 180 "parser.mly"
  (
			IF_THEN(_2,_4,IF_THEN(_6,_8,NOP))
		)
# 525 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 187 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "x out of range";
			(_2, _4)
		)
# 537 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'f) in
    Obj.repr(
# 196 "parser.mly"
  ( _1 )
# 544 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'f) in
    Obj.repr(
# 199 "parser.mly"
    (BINOP (OP_ADD,_1,_3))
# 552 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'f) in
    Obj.repr(
# 202 "parser.mly"
    (BINOP (OP_SUB,_1,_3) )
# 560 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 't) in
    Obj.repr(
# 209 "parser.mly"
  ( _1 )
# 567 "parser.ml"
               : 'f))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'f) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 't) in
    Obj.repr(
# 212 "parser.mly"
  ( BINOP (OP_MUL,_1,_3))
# 575 "parser.ml"
               : 'f))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'f) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 't) in
    Obj.repr(
# 215 "parser.mly"
  ( BINOP (OP_DIV,_1,_3) )
# 583 "parser.ml"
               : 'f))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'f) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 't) in
    Obj.repr(
# 218 "parser.mly"
  ( BINOP (OP_MOD,_1,_3) )
# 591 "parser.ml"
               : 'f))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 223 "parser.mly"
  ( CELL (0, fst _1, snd _1) )
# 598 "parser.ml"
               : 't))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 225 "parser.mly"
  (CST (_1))
# 605 "parser.ml"
               : 't))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 227 "parser.mly"
  (
		if (get_var _1) = -1 then VAR (declare_var _1)
		else VAR (get_var _1)
		)
# 615 "parser.ml"
               : 't))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 233 "parser.mly"
    ( _2 )
# 622 "parser.ml"
               : 't))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 't) in
    Obj.repr(
# 235 "parser.mly"
  ( _2 )
# 629 "parser.ml"
               : 't))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 't) in
    Obj.repr(
# 237 "parser.mly"
  ( NEG (_2) )
# 636 "parser.ml"
               : 't))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
