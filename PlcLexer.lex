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
        %%
        %header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));

%%
