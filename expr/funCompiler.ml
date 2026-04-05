open FunExpr
open Utils

let parse_eval file =
  print_string ("File " ^ file ^ " is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    Location.init lexbuf file;
    begin
      try
        let expr_prog = Parser.expression Lexer.token lexbuf in
        let pfx_prog = 0, ToPfx.generate expr_prog in
        print_endline (FunPfx.Ast.string_of_program pfx_prog);
        FunPfx.Eval.eval_program pfx_prog []
      with
      | Parser.Error ->
         print_string "Syntax error: ";
         Location.print (Location.curr lexbuf)
      | Location.Error (e, l) ->
         print_string e;
         Location.print l
    end;
    close_in input_file
  with Sys_error _ ->
    print_endline ("Can't find file '" ^ file ^ "'")

let _ =
  Arg.parse [] parse_eval ""