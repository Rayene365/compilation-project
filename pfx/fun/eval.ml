open Ast
open Printf

type value =
  | VInt of int
  | VCode of command list

let rec string_of_value = function
  | VInt n -> string_of_int n
  | VCode cmds -> "(" ^ String.concat " " (List.map string_of_command cmds) ^ ")"

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_value stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

let rec nth_value depth stack =
  match depth, stack with
  | 0, v :: _ -> Some v
  | n, _ :: stack' when n > 0 -> nth_value (n - 1) stack'
  | _ -> None

(* Question 4.2 *)
let step state =
  match state with
  | [], _ ->
      Error ("Nothing to step", state)

  (* push *)
  | Push n :: q, stack ->
      Ok (q, VInt n :: stack)

  (* executable sequence *)
  | ExecSeq cmds :: q, stack ->
      Ok (q, VCode cmds :: stack)

  (* pop *)
  | Pop :: q, _ :: stack' ->
      Ok (q, stack')
  | Pop :: _, _ ->
      Error ("pop on empty stack", state)

  (* swap *)
  | Swap :: q, x :: y :: stack' ->
      Ok (q, y :: x :: stack')
  | Swap :: _, _ ->
      Error ("swap needs at least two elements", state)

  (* add *)
  | Add :: q, VInt x :: VInt y :: stack' ->
      Ok (q, VInt (x + y) :: stack')
  | Add :: _, _ ->
      Error ("add needs two integers", state)

  (* sub : top - second *)
  | Sub :: q, VInt x :: VInt y :: stack' ->
      Ok (q, VInt (x - y) :: stack')
  | Sub :: _, _ ->
      Error ("sub needs two integers", state)

  (* mul *)
  | Mul :: q, VInt x :: VInt y :: stack' ->
      Ok (q, VInt (x * y) :: stack')
  | Mul :: _, _ ->
      Error ("mul needs two integers", state)

  (* div : top / second *)
  | Div :: _, VInt _ :: VInt 0 :: _ ->
      Error ("division by zero", state)
  | Div :: q, VInt x :: VInt y :: stack' ->
      Ok (q, VInt (x / y) :: stack')
  | Div :: _, _ ->
      Error ("div needs two integers", state)

  (* rem : top mod second *)
  | Rem :: _, VInt _ :: VInt 0 :: _ ->
      Error ("modulo by zero", state)
  | Rem :: q, VInt x :: VInt y :: stack' ->
      Ok (q, VInt (x mod y) :: stack')
  | Rem :: _, _ ->
      Error ("rem needs two integers", state)

  (* exec *)
  | Exec :: q, VCode cmds :: stack' ->
      Ok (cmds @ q, stack')
  | Exec :: _, _ ->
      Error ("exec needs an executable sequence on top of the stack", state)

  (* get *)
  | Get :: _, VInt i :: _ when i < 0 ->
      Error ("get needs a non-negative index", state)
  | Get :: q, VInt i :: stack' ->
      begin
        match nth_value i stack' with
        | Some v -> Ok (q, v :: stack')
        | None -> Error ("get index is too large", state)
      end
  | Get :: _, _ ->
      Error ("get needs an integer on top of the stack", state)


let eval_program (numargs, cmds) args =
  let rec execute = function
    | [], []    -> Ok None
    | [], v::_  -> Ok (Some v)
    | state ->
       begin
         match step state with
         | Ok s    -> execute s
         | Error e -> Error e
       end
  in
  if numargs = List.length args then
    match execute (cmds, List.map (fun i -> VInt i) args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %s\n" (string_of_value result)
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"
