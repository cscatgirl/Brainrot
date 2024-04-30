{open Parser
 let string_func s =
Scanf.sscanf("\"" ^ s ^ "\"") "%S%!" (fun y ->y )
}
let printable = [' ' -'~']
let escapes = "\\n" | "\\t"  | "\\v"
            |"\\b"  | "\\r"  | "\\f"
            |"\\a"  | "\\\\" | "\\?"
                        |"\\'" | "\\\""
let string = '"' ((printable|escapes)* as str) '"'

rule token = parse
[' ' '\t' '\r' '\n'] { token lexbuf}
| "**" {comment lexbuf}
| '(' {LPAREN}
| ')' {RPAREN}
| '{' {LBRACE}
| '}' {RBRACE}
| ";" {SEMI}
| '+' {PLUS}
| ','      { COMMA }
| '-' {MINUS}
| '/' {DIVIDE}
| '*' {TIMES}
| '<'         { LT }
| "<="        { LEQ }
| '>'         { GT }
| ">=" { GEQ }
| "kisses" {MOD}
| "uwu" {ASSIGN}
| "UwU" {EQ}
| "OwO" {NEQ}
|"&&" {AND}
| "||" {OR}
| "Nwo" {NOT}
|"nyaa" {IF}
| "nyaaa" {ELSE}
| "fwor" {FOR}
| "while" {WHILE}
| "byez" {RETURN}
| "intwet" {INT}
| "bwool" {BOOL}
| "Wordz" {STRING}
| "Emwpty" {VOID}
| "twuoo"     {TRUE}
| "fwalse"   {FALSE}
|['0'-'9']+ as lxm { INTLIT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as lxm { ID(lxm) }
| string {STRINGLIT(string_func str )}
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
| "**" {token lexbuf}
| _ {comment lexbuf}
