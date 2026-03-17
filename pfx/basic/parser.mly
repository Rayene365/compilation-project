%{
  open Ast
%}

(**************
 * The tokens *
 **************)

(* enter tokens here, they should begin with %token *)
%token EOF
%token <int> INT
%token PUSH POP SWAP ADD SUB MUL DIV REM


(******************************
 * Entry points of the parser *
 ******************************)

(* enter your %start clause here *)
%start <Ast.program> program
%type <Ast.command list> commands
%type <Ast.command> command

%%

(*************
 * The rules *
 *************)

program:
  | i=INT cmds=commands EOF { i, cmds }

commands:
  | c=command cmds=commands { c :: cmds }
  |                         { [] }

command:
  | PUSH i=INT { Push i }
  | POP        { Pop }
  | SWAP       { Swap }
  | ADD        { Add }
  | SUB        { Sub }
  | MUL        { Mul }
  | DIV        { Div }
  | REM        { Rem }

%%