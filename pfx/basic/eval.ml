open Ast
open Printf

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_int stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

(* Question 4.2 *)
let step state =
  match state with
  | [], _ ->
      Error ("Nothing to step", state)

  (* push *)
  | Push n :: q, stack ->
      Ok (q, n :: stack)

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
  | Add :: q, x :: y :: stack' ->
      Ok (q, (x + y) :: stack')
  | Add :: _, _ ->
      Error ("add needs at least two elements", state)

  (* sub : top - second *)
  | Sub :: q, x :: y :: stack' ->
      Ok (q, (x - y) :: stack')
  | Sub :: _, _ ->
      Error ("sub needs at least two elements", state)

  (* mul *)
  | Mul :: q, x :: y :: stack' ->
      Ok (q, (x * y) :: stack')
  | Mul :: _, _ ->
      Error ("mul needs at least two elements", state)

  (* div : top / second *)
  | Div :: _, _ :: 0 :: _ ->
      Error ("division by zero", state)
  | Div :: q, x :: y :: stack' ->
      Ok (q, (x / y) :: stack')
  | Div :: _, _ ->
      Error ("div needs at least two elements", state)

  (* rem : top mod second *)
  | Rem :: _, _ :: 0 :: _ ->
      Error ("modulo by zero", state)
  | Rem :: q, x :: y :: stack' ->
      Ok (q, (x mod y) :: stack')
  | Rem :: _, _ ->
      Error ("rem needs at least two elements", state)


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
    match execute (cmds,args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"
