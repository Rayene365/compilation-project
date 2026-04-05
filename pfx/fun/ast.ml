type command =
  | Push of int
  | Pop
  | Swap
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | ExecSeq of command list
  | Exec
  | Get

type program = int * command list

(* add here all useful functions and types  related to the AST: for instance  string_of_ functions *)

let rec string_of_command = function
  | Push n -> Printf.sprintf "push %d" n
  | Pop -> "pop"
  | Swap -> "swap"
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Rem -> "rem"
  | ExecSeq cmds -> "(" ^ string_of_commands cmds ^ ")"
  | Exec -> "exec"
  | Get -> "get"

and string_of_commands cmds = String.concat " " (List.map string_of_command cmds)

let rec string_of_command_ast = function
  | Push n -> Printf.sprintf "Push %d" n
  | Pop -> "Pop"
  | Swap -> "Swap"
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Rem -> "Rem"
  | ExecSeq cmds ->
      let printed_cmds = String.concat "; " (List.map string_of_command_ast cmds) in
      Printf.sprintf "ExecSeq [%s]" printed_cmds
  | Exec -> "Exec"
  | Get -> "Get"

let string_of_program (args, cmds) =
  let printed_cmds = String.concat "; " (List.map string_of_command_ast cmds) in
  Printf.sprintf "(%d, [%s])" args printed_cmds
