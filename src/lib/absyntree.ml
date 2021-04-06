(* Convert abstract syntax trees to generic trees of list of string *)

open Absyn

(* Helper functions *)

let name = Symbol.name
let map = List.map
let sprintf = Format.sprintf

(* Concatenate the lines of text in the node *)
let flat_nodes tree =
  Tree.map (String.concat "\n") tree

(* Build a singleton tree from a string *)
let mktr s = Tree.mkt [s]

(* Convert a symbol to a general tree *)
let tree_of_symbol s = mktr (Symbol.name s) []

(* Convert a binary operator to a string *)
let stringfy_operator op =
  match op with
  | Plus -> "+"
  | LT -> "<"
  | EQ -> "="

(* Convert an expression to a generic tree *)
let rec tree_of_exp exp =
  match exp with
  | IntExp x -> mktr (sprintf "IntExp %i" x) []
  | OpExp (op, l, r) -> mktr (sprintf "OpExp %s" (stringfy_operator op)) [tree_of_lexp l; tree_of_lexp r]
  | IdExp x -> mktr (sprintf "IdExp %s" (name x)) []
  | CondExp (exp1, exp2, exp3)-> mktr (sprintf "CondExp if") [tree_of_lexp exp1; tree_of_lexp exp2; tree_of_lexp exp3]
  | FuncCallExp (x, exp) -> mktr (sprintf "FuncCallExp %s" (name x)) (List.map tree_of_lexp exp)
  | DeclarationExp (x, exp1, exp2) -> mktr (sprintf "DeclarationExp %s" (name x)) [tree_of_lexp exp1; tree_of_lexp exp2]

and tree_of_fundec (typeid, params, body) =
  mktr
    "Fun"
    [ tree_of_typeid typeid;
      mktr "Formals" (List.map tree_of_typeid params);
      tree_of_lexp body
    ]

and tree_of_typeid (type_, id) =
  mktr (sprintf "%s:%s" (name id) (show_type_ type_)) []

(* Convert an anotated expression to a generic tree *)
and tree_of_lexp (_, x) = tree_of_exp x

and tree_of_lfundec (_, x) = tree_of_fundec x

and tree_of_lsymbol (_, x) = tree_of_symbol x

and tree_of_lfundecs (_, lfuncs) = mktr "Program tree" (List.map tree_of_lfundec lfuncs)