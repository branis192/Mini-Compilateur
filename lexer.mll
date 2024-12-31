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

 {

open Common
open Parser
open Printf

let line = ref 1
}

let digit = ['0'-'9']
let sign = ['+' '-']
let dec = sign? digit+
let id = ['a' - 'z']['a' - 'z''A'-'Z''0'-'9''_'':']*

rule token = parse
	'\n'			{ incr line; token lexbuf }
|	[' ' '\t' '\r']	{ token lexbuf }
|	"//"			{ ecom lexbuf }
|	"#"				{ ecom lexbuf }
|	'&'				{ AND }
|	'|'				{ OR }
|	'!'				{ NOT }
|	'+'		{ PLUS }
|	"-"		{ MOINS }
|	"dimensions"	{ DIMENSIONS }
|	"*"		{ MULT }
|	"%"		{ MOD }
|	"/"		{ DIV }
|	"elsif"		{ ELSIF }
|	"end"			{ END }
|	"of"			{ OF }
|	":="			{ ASSIGN }
|	'='			{ EQ }
|	"!="			{ NE }
|	"<="			{ LE }
|	">="			{ GE }
|	'<'			{ LT }
|	'>'			{ GT }
|	"if"			{ IF }
|	"else"			{ ELSE }
|	"then"			{ THEN }
|	'.'				{ DOT }
|	".."			{ DOT_DOT}
|	','				{ COMMA }
|	'['				{ LBRACKET }
|	']'				{ RBRACKET }
|	'('				{ LPAR }
|	')'				{ RPAR }
|	dec	as n		{ INT (int_of_string n) }
|	id  as k 	 		{ ID k }

|	eof				{ EOF }
|	_ as c			{ raise (LexerError (sprintf "illegal char '%c'" c)) }

and ecom = parse
|	'\n'			{ incr line; token lexbuf }
|	eof				{ EOF }
|	_				{ ecom lexbuf }
