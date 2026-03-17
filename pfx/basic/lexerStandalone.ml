open BasicPfx
open Utils

let print_token = function
  | Parser.EOF -> print_string "EOF"
  | Parser.INT i -> print_int i
  | Parser.PUSH -> print_string "PUSH"
  | Parser.POP -> print_string "POP"
  | Parser.SWAP -> print_string "SWAP"
  | Parser.ADD -> print_string "ADD"
  | Parser.SUB -> print_string "SUB"
  | Parser.MUL -> print_string "MUL"
  | Parser.DIV -> print_string "DIV"
  | Parser.REM -> print_string "REM"

let rec examine_all lexbuf =
  let result = Lexer.token lexbuf in
  print_token result;
  print_string " ";
  match result with
  | Parser.EOF -> ()
  | _ -> examine_all lexbuf

let compile file =
  print_string ("File " ^ file ^ " is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    Location.init lexbuf file;
    begin
      try
        examine_all lexbuf;
        print_newline ()
      with Location.Error (e, l) ->
        print_string e;
        Location.print l
    end;
    close_in input_file
  with Sys_error _ ->
    print_endline ("Can't find file '" ^ file ^ "'")

let _ =
  Arg.parse [] compile ""
