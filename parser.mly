/*
 * autocell - AutoCell compiler and viewer
 * Copyright (C) 2021  University of Toulouse, France <casse@irit.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

%{

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

%}

%token EOF

/* keywords */
%token DIMENSIONS

%token END
%token OF

/* symbols */
%token ASSIGN
%token COMMA
%token LBRACKET RBRACKET
%token LPAR RPAR
%token DOT_DOT
%token DOT
%token PLUS
%token MOINS
%token MULT
%token MOD
%token DIV
%token NON
%token IF
%token THEN
%token ELSE
%token ELSIF
%token GT
%token GE
%token LT
%token LE
%token EQ
%token NE
%token OR
%token NOT
%token AND

/* values */
%token <string> ID
%token<int> INT

%start program
%type<Ast.prog> program

%%

program: INT DIMENSIONS OF config END opt_statements EOF
	{
		if $1 != 2 then error "only 2 dimension accepted";
		($4, $6)
	}
;

config:
	INT DOT_DOT INT
		{
			if $1 >= $3 then error "illegal field values";
			[("", (0, ($1, $3)))]
		}
|	fields
		{ set_fields $1 }
;

fields:
	field
		{ [$1] }
		
|	fields COMMA field
		{$3 :: $1 }
;

field:
	ID OF INT DOT_DOT INT
		{
			if $3 >= $5 then error "illegal field values";
			($1, ($3, $5))
		}
;

cond:
	aux_cond1
		{	$1	}
|	cond OR aux_cond1 
		{	OR( $1,$3 )	}
;
aux_cond1:
	aux_cond
		{	$1	}
|	aux_cond AND aux_cond1
		{	AND( $1,$3 )	}
;
aux_cond:
	expression EQ expression
	{	COMP(COMP_EQ,$1,$3)	}
|	expression GE expression
	{	COMP(COMP_GE,$1,$3)	}
|	expression GT expression
	{	COMP(COMP_GT,$1,$3)	}
|	expression LT expression
	{	COMP(COMP_LT,$1,$3)	}
|	expression LE expression
	{	COMP(COMP_LT,$1,$3)	}
|	expression NE expression
	{	COMP(COMP_NE,$1,$3)	}
|	NOT aux_cond
	{	NOT $2 	}
|	LPAR cond RPAR
	{	$2	}
;

	
opt_statements:
	statement
		{ $1 }
|	opt_statements statement 
		{ SEQ($1, $2) }
		
;

statement:
	cell ASSIGN expression
		{
			if (fst $1) != 0 then error "assigned x must be 0";
			if (snd $1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, $3)
		}
|	ID ASSIGN expression
		{ 
			if (get_var $1) = -1 then SET_VAR ((declare_var $1),$3) 
			else SET_VAR ((get_var $1),$3)		
		}
|	IF cond THEN opt_statements END
		{
			IF_THEN($2,$4,NOP)
		}
|	IF cond THEN opt_statements ELSE opt_statements END
		{
			IF_THEN($2,$4,$6)
		}
|	IF cond THEN opt_statements ELSIF cond THEN opt_statements END
		{
			IF_THEN($2,$4,IF_THEN($6,$8,NOP))
		}	
;

cell:
	LBRACKET INT COMMA INT RBRACKET
		{
			if ($2 < -1) || ($2 > 1) then error "x out of range";
			if ($4 < -1) || ($4 > 1) then error "x out of range";
			($2, $4)
		}
;

expression:
	f
		{ $1 }
		
| expression PLUS f
    {BINOP (OP_ADD,$1,$3)}

| expression MOINS f
    {BINOP (OP_SUB,$1,$3) }
    
;


f:
	t
		{ $1 }
		
|	f MULT t 
		{ BINOP (OP_MUL,$1,$3)}

|	f DIV t 
		{ BINOP (OP_DIV,$1,$3) }

|	f MOD t 
		{ BINOP (OP_MOD,$1,$3) }
;		

t:
	cell
		{ CELL (0, fst $1, snd $1) }
|	INT
		{CST ($1)}
|	ID 
		{
		if (get_var $1) = -1 then VAR (declare_var $1)
		else VAR (get_var $1)
		}
						
|	LPAR expression RPAR
    { $2 }
|	PLUS t
		{ $2 }
|	MOINS t
		{ NEG ($2) }
;
