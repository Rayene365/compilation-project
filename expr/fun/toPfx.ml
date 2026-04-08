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

let union xs ys =
  List.fold_left (fun acc x -> if List.mem x acc then acc else acc @ [x]) xs ys

let rec free_vars bound = function
  | Const _ -> []
  | Var var -> if List.mem var bound then [] else [var]
  | Uminus e -> free_vars bound e
  | Binop (_, e1, e2)
  | App (e1, e2) ->
      union (free_vars bound e1) (free_vars bound e2)
  | Fun (var, body) ->
      free_vars (var :: bound) body

let function_env free_vars param =
  List.mapi (fun depth var -> var, depth) free_vars @ [param, List.length free_vars]

let capture_code env free_vars =
  List.concat_map
    (fun var ->
      let depth = lookup env var + 1 in
      [Push depth; Get; Append])
    free_vars

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
      let free = free_vars [var] body in
      let body_env = function_env free var in
      let body_code = generate_into body_env body [] in
      ExecSeq body_code :: capture_code env free @ cont
  | App (e1, e2) ->
      generate_into env e2 (generate_into (shift_env env) e1 (Exec :: Swap :: Pop :: cont))

let generate expr =
  generate_into [] expr []
