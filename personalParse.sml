structure PlcFELrVals = PlcParserLrValsFun(structure Token = LrParser.Token)
structure PlcLexer = PlcLexerFun(structure Tokens = PlcFELrVals.Tokens);
structure PlcParser =
    Join(structure LrParser = LrParser
        structure ParserData = PlcFELrVals.ParserData
        structure Lex = PlcLexer)

fun invoke lexstream =
        let fun print_error (s,i:int,_) =
                    TextIO.output(TextIO.stdOut,
                        "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
        in PlcParser.parse(0,lexstream,print_error,())
        end

fun parse () = 
        let val lexer = PlcParser.makeLexer(
                    fn _ => valOf(TextIO.inputLine TextIO.stdIn))
                                        
            val dummyEOF = PlcFELrVals.Tokens.EOF(0,0)
                                            
            fun loop lexer =
                    let val (result,lexer) = invoke lexer
                        val (nextToken,lexer) = PlcParser.Stream.get lexer
                    in if PlcParser.sameToken(nextToken, dummyEOF) then ()
                        else loop lexer
                    end
        in loop lexer
        end

