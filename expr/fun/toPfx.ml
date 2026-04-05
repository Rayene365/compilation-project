open Ast
open FunPfx.Ast
open BinOp

type env = (string * int) list

let lookup env var =
  try List.assoc var env
  with Not_found -> failwith ("unbound variable " ^ var)

let shift_env env =
  List.map (fun (var, depth) -> var, depth + 1) env

let extend_env env var =
  (var, 0) :: shift_env env

let opcode_of_binop = function
  | Badd -> Add
  | Bsub -> Sub
  | Bmul -> Mul
  | Bdiv -> Div
  | Bmod -> Rem

let rec generate_into env expr cont =
  match expr with
  | Const n ->
      Push n :: cont
  | Var var ->
      let depth = lookup env var in
      Push depth :: Get :: cont
  | Uminus e ->
      generate_into env e (Push 0 :: Sub :: cont)
  | Binop (op, e1, e2) ->
      generate_into env e2 (generate_into (shift_env env) e1 (opcode_of_binop op :: cont))
  | Fun (var, body) ->
      ExecSeq (generate_into (extend_env env var) body []) :: cont
  | App (e1, e2) ->
      generate_into env e2 (generate_into (shift_env env) e1 (Exec :: Swap :: Pop :: cont))

let generate expr =
  generate_into [] expr []