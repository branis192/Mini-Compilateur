(*
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
 *)

open Ast
open Cell
open Quad
open Symbols

(** Variable containing the current x position. *)
let x = 0

(** Variable containing the current y position. *)
let y = 1

(** Variable containing the width of the array. *)
let w = 2

(** Variable containing the height of the array. *)
let h = 3

(** Variable containing 1! *)
let one = 4

(** Compute the position from the relative offset.
	@param x	X offset.
	@param y	Y offset.
	@return		Corresponding position. *)
let pos x y =
	match (x, y) with
	| (0, 0)	-> pCENTER
	| (0, -1)	-> pNORTH
	| (-1, -1)	-> pNORTHWEST
	| (-1, 0)	-> pWEST
	| (-1, +1)	-> pSOUTHWEST
	| (0, +1)	-> pSOUTH
	| (+1, +1)	-> pSOUTHEAST
	| (+1, 0)	-> pEAST
	| (+1, -1)	-> pNORTHEAST
	| _			-> failwith "bad offsets"
	


(** Compile an expression.
	@param e	Expression to compile.
	@return		(register containing the result, quads producing the result). *)
let rec comp_expr e =


	match e with
	| NONE ->
		(0, [])
	| CELL (f, x, y) ->
		let v = new_reg () in
		(v, [
			INVOKE (cGET + f, v, pos x y)
		])
	| CST (i) ->
		let v = new_reg () in
		(v, [SETI (v,i)])
			
	| VAR (x) ->
		(x, [])
		
	| NEG (x) ->
		let (v, c) = comp_expr e in
		let reg_neg = new_reg() in 
		let zer = new_reg() in 
		SETI (zer,0);
		(reg_neg,c @ [SUB(reg_neg,zer,v)])
		
	  
	| BINOP(op, e1, e2) ->
	    let (reg_e1, code_e1) = comp_expr e1 in
	    let (reg_e2, code_e2) = comp_expr e2 in
	    let reg_res = new_reg() in
	    let op_quad = match op with
	      | OP_ADD -> ADD(reg_res, reg_e1, reg_e2)
	      | OP_SUB -> SUB(reg_res, reg_e1, reg_e2)
	      | OP_MUL -> MUL(reg_res, reg_e1, reg_e2)
	      | OP_DIV -> DIV(reg_res, reg_e1, reg_e2)
	      | OP_MOD -> MOD(reg_res, reg_e1, reg_e2)
	    in
	    (reg_res, code_e1 @ code_e2 @ [op_quad])

	| _ ->
		failwith "bad expression"


(** Compile a condition.
	@param c		Condition to compile.
	@param l_then	Label to branch to when the condition is true.
	@param l_else	Label to branch to when the condition is false.
	@return			Quads implementing the condition. *)
let rec comp_cond c l_then l_else =


	match c with
	| COMP(COMP_EQ,v1,v2) ->
		let (r1,q1) = comp_expr v1 in
		let (r2,q2) = comp_expr v2 in 
		q1 @ q2 @ [GOTO_EQ(l_then,r1,r2);GOTO(l_else)] 
	| COMP(COMP_GE,v1,v2) ->
		let (r1,q1) = comp_expr v1 in
		let (r2,q2) = comp_expr v2 in 
		q1 @ q2 @ [GOTO_GE(l_then,r1,r2);GOTO(l_else)] 
	| COMP(COMP_LT,v1,v2) ->
		let (r1,q1) = comp_expr v1 in
		let (r2,q2) = comp_expr v2 in 
		q1 @ q2 @ [GOTO_LT(l_then,r1,r2);GOTO(l_else)] 
	| COMP(COMP_LE,v1,v2) ->
		let (r1,q1) = comp_expr v1 in
		let (r2,q2) = comp_expr v2 in 
		q1 @ q2 @ [GOTO_LE(l_then,r1,r2);GOTO(l_else)] 
	| COMP(COMP_NE,v1,v2) ->
		let (r1,q1) = comp_expr v1 in
		let (r2,q2) = comp_expr v2 in 
		q1 @ q2 @ [GOTO_NE(l_then,r1,r2);GOTO(l_else)] 
	| COMP(COMP_GT,v1,v2) ->
		let (r1,q1) = comp_expr v1 in
		let (r2,q2) = comp_expr v2 in 
		q1 @ q2 @ [GOTO_GT(l_then,r1,r2);GOTO(l_else)] 

	| NOT c1 ->
		comp_cond c1 l_then l_else

	| AND(c1,c2) ->
		let label = new_lab() in
		let q1 = comp_cond c1 l_then l_else in
		let q2 = comp_cond c2 l_then l_else in
		q1 @ [LABEL label] @ q2 

	| OR(c1,c2) ->
		let label = new_lab() in
		let q1 = comp_cond c1 l_then l_else in
		let q2 = comp_cond c2 l_then l_else in
		q1 @ [LABEL label] @ q2 
	| _ ->
		failwith "bad condition"


(** Compile a statement.
	@param s	Statement to compile.
	@return		Quads implementing the statement. *)
let rec comp_stmt s =
	match s with
	| NOP ->
		[]
	| SEQ (s1, s2) ->
		(comp_stmt s1) @ (comp_stmt s2)
	| SET_CELL (f, e) ->
		let (v, q) = comp_expr e in
		q @ [
			INVOKE (cSET, v, f)
		]
	| SET_VAR (x,y) -> 
		let (v, c) = comp_expr y in
		let reg_var = x in 
		c @ [SET(reg_var,v)]
	
	| IF_THEN (c,s1,s2) ->
		let lab_then = new_lab() in
		let lab_else = new_lab() in 
		let lab_end = new_lab() in 
		let lis1 = comp_cond c lab_then lab_else in
		let lis2 = comp_stmt s1 in
		let lis3 = comp_stmt s2 in
		lis1 @ [LABEL lab_then] @ lis2 @ [GOTO lab_end] @ [LABEL lab_else] @ lis3 @ [LABEL lab_end]
		
	| _ ->
		failwith "bad instruction"

(** Compile the given application.
	@param flds		List of fields.
	@param stmt		Instructions.
	@return			List of quads. *)	
let compile flds stmt =
	let x_lab = new_lab () in
	let y_lab = new_lab () in
	[
		INVOKE(cSIZE, w, h);
		SETI(one, 1);

		SETI(x, 0);
		LABEL x_lab;

		SETI(y, 0);
		LABEL y_lab;
		INVOKE(cMOVE, x, y)
	]
	@
	(comp_stmt stmt)
	@
	[
		ADD(y, y, one);
		GOTO_LT(y_lab, y, h);

		ADD(x, x, one);
		GOTO_LT(x_lab, x, w);
		STOP
	]
