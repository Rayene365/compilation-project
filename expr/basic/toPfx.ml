open Ast
open BasicPfx.Ast
open BinOp

let rec generate_into expr cont =
  match expr with
  | Const n ->
      Push n :: cont
  | Uminus e ->
      generate_into e (Push 0 :: Sub :: cont)
  | Binop (Badd, e1, e2) ->
      generate_into e2 (generate_into e1 (Add :: cont))
  | Binop (Bsub, e1, e2) ->
      generate_into e2 (generate_into e1 (Sub :: cont))
  | Binop (Bmul, e1, e2) ->
      generate_into e2 (generate_into e1 (Mul :: cont))
  | Binop (Bdiv, e1, e2) ->
      generate_into e2 (generate_into e1 (Div :: cont))
  | Binop (Bmod, e1, e2) ->
      generate_into e2 (generate_into e1 (Rem :: cont))
  | Var _ ->
      failwith "variables are not supported yet"

let generate expr = generate_into expr []
