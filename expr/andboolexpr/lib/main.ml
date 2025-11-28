open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | Not(e) -> "Not(" ^ (string_of_boolexpr e) ^ ")"
  | And(e1,e2) -> "And(" ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | Or(e1,e2) -> "Or(" ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"



let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec trace1 e = match e with
        If(True,e1,_) -> e1
      | If(False,_,e2) -> e2
      | If(e1,e2,e3) -> let e' = trace1 e1 in If(e',e2,e3)
      | Not(e1) -> Not(trace1 e1)
      | Or(e1,e2) -> Or(trace1 e1,e2)
      | And(e1,e2) -> if (e1 = True) then e2 else (if (e1 = False) then False else And(trace1 e1,e2))
      | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval b = match b with
      | True -> true
      | False -> false
      | Not(b) -> not (eval b)
      | And(b1,b2) -> eval b1 && eval b2
      | Or(b1,b2) -> eval b1 || eval b2
      | If(b1, b2, b3) -> if (eval b1) then (eval b2) else (eval b3)
