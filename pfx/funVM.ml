open FunPfx
open Utils

let args = ref []

let parse_eval file =
  print_string ("File " ^ file ^ " is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    Location.init lexbuf file;
    begin
      try
        let pfx_prog = Parser.program Lexer.token lexbuf in
        print_endline (Ast.string_of_program pfx_prog);
        Eval.eval_program pfx_prog !args
      with
      | Parser.Error ->
         print_string "Syntax error: ";
         Location.print (Location.curr lexbuf)
      | Location.Error (e, l) ->
         print_string e;
         Location.print l
    end;
    close_in input_file
  with Sys_error _ -> print_endline ("Can't find file '" ^ file ^ "'")

let _ =
  let register_arg i = args := !args @ [i] in
  Arg.parse ["-a", Arg.Int register_arg, "integer argument"] parse_eval ""
