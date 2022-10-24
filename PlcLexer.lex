(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* Define what to do when the end of the file is reached. *)
val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)

(* A function to print a message error on the screen. *)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
                "line ", (Int.toString l), ": ", e, "\n"
            ])
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
        let
            val lineNum = !lineNumber
        in
            Int.toString lineNum
        end

(* Initialize the lexer. *)
fun init() = ()

(* | OBRACKET Prog CBRACKET (Prog) *)

(* DEFINITIONS *)
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
digit   = [0-9];
ws      = [\ \t];
name   = [A-Za-z_][A-Za-z_0-9]*^[__];

%%
\n       	=> (pos := (!pos) + 1; Tokens.EOF(!pos, !pos));
";"      	=> (Tokens.SEMI(!pos, !pos));
","      	=> (Tokens.COMMA(!pos, !pos));
"("      	=> (Tokens.OPAR(!pos,!pos));
")"      	=> (Tokens.CPAR(!pos,!pos));
"["         => (Tokens.OBRACE(!pos,!pos));
"]"         => (Tokens.CBRACE(!pos,!pos));
"{"         => (Tokens.OBRACKET(!pos,!pos));
"}"         => (Tokens.CBRACKET(!pos,!pos));

{ws}+    	=> (lex());
\(\*.*\*\) 	=> (lex());

"if"        => (Tokens.IF(!pos,!pos));
"then"      => (Tokens.THEN(!pos,!pos));
"else"      => (Tokens.ELSE(!pos,!pos));
"match"     => (Tokens.MATCH(!pos,!pos));
"with"      => (Tokens.WITH(!pos,!pos));
"!"         => (Tokens.NOT(!pos,!pos));
"hd"        => (Tokens.HD(!pos,!pos));
"tl"        => (Tokens.TL(!pos,!pos));
"ise"       => (Tokens.ISE(!pos,!pos));
"print"     => (Tokens.PRINT(!pos,!pos));
"&&"        => (Tokens.AND(!pos,!pos));
"+"      	=> (Tokens.PLUS(!pos,!pos));
"-"      	=> (Tokens.MINUS(!pos,!pos));
"*"      	=> (Tokens.TIMES(!pos,!pos));
"/"      	=> (Tokens.DIV(!pos,!pos));
"="      	=> (Tokens.EQ(!pos,!pos));
"!="     	=> (Tokens.DIF(!pos,!pos));
"<"      	=> (Tokens.LESS(!pos,!pos));
"<="     	=> (Tokens.LESSEQ(!pos,!pos));
"::"     	=> (Tokens.INFIX(!pos,!pos));

"true"   	=> (Tokens.BOOL(true, !pos, !pos));
"false" 	=> (Tokens.BOOL(false, !pos, !pos));

"Nil" 	    => (Tokens.NIL_T(!pos, !pos));
"Bool" 	    => (Tokens.BOOL_T(!pos, !pos));
"Int" 	    => (Tokens.INT_T(!pos, !pos));

"fn"      	=> (Tokens.FN(!pos, !pos));
"fun"      	=> (Tokens.FUN(!pos, !pos));
"end"      	=> (Tokens.END(!pos, !pos));
"rec"      	=> (Tokens.REC(!pos, !pos));
"->"      	=> (Tokens.ARROW(!pos, !pos));

"__"        => (lex());
"var"      	=> (Tokens.VAR(!pos, !pos));

{name}      => (Tokens.NAME(yytext, !pos, !pos));
{digit}+ 	=> (Tokens.NUM(valOf (Int.fromString yytext), !pos, !pos));
.        	=> (error ("ignoring bad character " ^ yytext, !pos, !pos); lex());

