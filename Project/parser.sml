(* 
 *   CODE FOR HOMEWORK 4
 *)


structure Parser =  struct

  (*
   *  Wrapper around the regexp library
   *)      

  structure R = RegExpFn (structure P = AwkSyntax structure E = DfaEngine)

  structure I = InternalRepresentation
                
  (* match a compiled regular expression against a list of characters *)
                
  fun matchRE' re cs = let
    val prefix = R.prefix re List.getItem
    fun getMatch NONE = NONE
      | getMatch (SOME (mt, cs')) = let
          val {pos,len} = MatchTree.root mt
        in
          SOME (implode (List.take (pos,len)), cs')
        end
  in
    getMatch (prefix cs)
  end
                       
  (* match a string regular expression against a list of characters *)
                       
  fun matchRE re cs = matchRE' (R.compileString re) cs



  exception Parsing of string

  val err = ref "unknown error"
  val funErr = ref ""


  fun parseError msg = (funErr :=  "";raise Parsing msg)
                         
                         


  (* 
   *   A simple lexer
   *
   *   Details in lecture 5
   *
   *   Modified to deal with keywords correctly
   *
   *)

  datatype token = T_LET 
                 | T_IN
                 | T_SYM of string 
                 | T_INT of int 
                 | T_TRUE 
                 | T_FALSE
                 | T_EQUAL
                 | T_LESS
                 | T_IF 
                 | T_THEN
                 | T_ELSE
                 | T_LPAREN 
                 | T_RPAREN
                 | T_PLUS
                 | T_MINUS
                 | T_TIMES
                 | T_BACKSLASH
                 | T_RARROW
                 | T_LARROW
                 | T_DCOLON
                 | T_COMMA
                 | T_LBRACKET
                 | T_RBRACKET
                 | T_LBRACE
                 | T_RBRACE
                 | T_DOT
                 | T_HASH
                 | T_DDOTS
                 | T_BAR
                 | T_MATCH
                 | T_WITH
     | T_DEF


  fun stringOfToken T_LET = "T_LET"
    | stringOfToken T_IN = "T_IN"
    | stringOfToken (T_SYM s) = "T_SYM["^s^"]"
    | stringOfToken (T_INT i) = "T_INT["^(Int.toString i)^"]"
    | stringOfToken T_TRUE = "T_TRUE"
    | stringOfToken T_FALSE = "T_FALSE"
    | stringOfToken T_EQUAL = "T_EQUAL"
    | stringOfToken T_LESS = "T_LESS"
    | stringOfToken T_IF  = "T_IF"
    | stringOfToken T_THEN  = "T_THEN"
    | stringOfToken T_ELSE  = "T_ELSE"
    | stringOfToken T_LPAREN = "T_LPAREN"
    | stringOfToken T_RPAREN = "T_RPAREN"
    | stringOfToken T_PLUS = "T_PLUS"
    | stringOfToken T_MINUS = "T_MINUS"
    | stringOfToken T_TIMES = "T_TIMES"
    | stringOfToken T_BACKSLASH = "T_BACKSLASH"
    | stringOfToken T_RARROW = "T_RARROW"
    | stringOfToken T_LARROW = "T_LARROW"
    | stringOfToken T_DCOLON = "T_DCOLON"
    | stringOfToken T_COMMA = "T_COMMA"
    | stringOfToken T_LBRACKET = "T_LBRACKET"
    | stringOfToken T_RBRACKET = "T_RBRACKET"
    | stringOfToken T_LBRACE = "T_LBRACE"
    | stringOfToken T_RBRACE = "T_RBRACE"
    | stringOfToken T_DOT = "T_DOT"
    | stringOfToken T_HASH = "T_HASH"
    | stringOfToken T_DDOTS = "T_DDOTS"
    | stringOfToken T_BAR = "T_BAR"
    | stringOfToken T_MATCH = "T_MATCH"
    | stringOfToken T_WITH = "T_WITH"
    | stringOfToken T_DEF = "T_DEF"


      fun stringOfTokenEnglish T_LET = "let"
    | stringOfTokenEnglish T_IN = "in"
    | stringOfTokenEnglish (T_SYM s) = s
    | stringOfTokenEnglish (T_INT i) = Int.toString i
    | stringOfTokenEnglish T_TRUE = "true"
    | stringOfTokenEnglish T_FALSE = "false"
    | stringOfTokenEnglish T_EQUAL = "="
    | stringOfTokenEnglish T_LESS = ">"
    | stringOfTokenEnglish T_IF  = "if"
    | stringOfTokenEnglish T_THEN  = "then"
    | stringOfTokenEnglish T_ELSE  = "else"
    | stringOfTokenEnglish T_LPAREN = "("
    | stringOfTokenEnglish T_RPAREN = ")"
    | stringOfTokenEnglish T_PLUS = "+"
    | stringOfTokenEnglish T_MINUS = "-"
    | stringOfTokenEnglish T_TIMES = "*"
    | stringOfTokenEnglish T_BACKSLASH = "/"
    | stringOfTokenEnglish T_RARROW = "->"
    | stringOfTokenEnglish T_LARROW = "<-"
    | stringOfTokenEnglish T_DCOLON = "::"
    | stringOfTokenEnglish T_COMMA = ","
    | stringOfTokenEnglish T_LBRACKET = "["
    | stringOfTokenEnglish T_RBRACKET = "]"
    | stringOfTokenEnglish T_LBRACE = "{"
    | stringOfTokenEnglish T_RBRACE = "}"
    | stringOfTokenEnglish T_DOT = "."
    | stringOfTokenEnglish T_HASH = "#"
    | stringOfTokenEnglish T_DDOTS = ".."
    | stringOfTokenEnglish T_BAR = "|"
    | stringOfTokenEnglish T_MATCH = "match"
    | stringOfTokenEnglish T_WITH = "with"
    | stringOfTokenEnglish T_DEF = "def"

                   
  fun whitespace _ = NONE
                     
  fun produceSymbol "let" = SOME (T_LET)
    | produceSymbol "in" = SOME (T_IN)
    | produceSymbol "true" = SOME (T_TRUE)
    | produceSymbol "false" = SOME (T_FALSE)
    | produceSymbol "if" = SOME (T_IF)
    | produceSymbol "then" = SOME (T_THEN)
    | produceSymbol "else" = SOME (T_ELSE)
    | produceSymbol "match" = SOME (T_MATCH)
    | produceSymbol "with" = SOME (T_WITH)
    | produceSymbol "def" = SOME (T_DEF)
    | produceSymbol text = SOME (T_SYM text)
                           
  fun produceInt text = (case Int.fromString text
                          of NONE => parseError "integer literal out of bounds"
                           | SOME i => SOME (T_INT i))
                        
  fun produceEqual _ = SOME (T_EQUAL)
  fun produceLess _ = SOME (T_LESS)
  fun produceLParen _ = SOME (T_LPAREN)
  fun produceRParen _ = SOME (T_RPAREN)

  fun producePlus _ = SOME (T_PLUS)
  fun produceMinus _ = SOME (T_MINUS)
  fun produceTimes _ = SOME (T_TIMES)
  fun produceComma _ = SOME (T_COMMA)

  fun produceBackslash _ = SOME (T_BACKSLASH)
  fun produceDColon _ = SOME (T_DCOLON)

  fun produceLArrow _ = SOME (T_LARROW)
  fun produceRArrow _ = SOME (T_RARROW)

  fun produceLBracket _ = SOME (T_LBRACKET)
  fun produceRBracket _ = SOME (T_RBRACKET)
  fun produceLBrace _ = SOME (T_LBRACE)
  fun produceRBrace _ = SOME (T_RBRACE)
  fun produceDDots _ = SOME (T_DDOTS)
  fun produceDot _ = SOME (T_DOT)
  fun produceHash _ = SOME (T_HASH)
  fun produceBar _ = SOME (T_BAR)
                       
  val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
  in
    map convert [("( |\\n|\\t)+",         whitespace),
                 ("=",                    produceEqual),
                 ("\\+",                  producePlus),
                 ("\\*",                  produceTimes),
                 ("\\\\",                 produceBackslash),
                 ("->",                   produceRArrow),
                 ("<-",                   produceLArrow),
                 ("<",                    produceLess),
                 ("-",                    produceMinus),
                 ("#",                    produceHash),
                 ("::",                   produceDColon),
                 ("\\.\\.",               produceDDots),
                 ("\\.",                  produceDot),
                 (",",                    produceComma),
                 ("\\|",                  produceBar),
                 ("[a-zA-Z][a-zA-Z0-9]*", produceSymbol),
                 ("~?[0-9]+",             produceInt),
                 ("\\(",                  produceLParen),
                 ("\\)",                  produceRParen),
                 ("{",                    produceLBrace),
                 ("}",                    produceRBrace),
                 ("\\[",                  produceLBracket),
                 ("\\]",                  produceRBracket)]
  end
               
               
  fun getToken cs = let
    fun loop [] = parseError ("cannot tokenize "^(implode cs))
      | loop ((re,f)::xs) = (case matchRE' re cs
                              of NONE => loop xs
                               | SOME (m,cs') => (f m,cs'))
  in
    loop tokens
  end
                    
                    
  fun lex []  = []
    | lex cs = let
        val (token,cs') = getToken cs
      in
        case token 
         of NONE => lex cs'
          | SOME t => t::(lex cs')
      end
               
               
  fun lexString str = lex (explode str)
                      
   

  val bulkerr = ref (lexString "")
  val soFar = ref ([]: token list)
  val needToken =  ref ([]: token list list)
  val savedSoFar = ref ([]: token list list)
  val stringVal = ref ([]: string list)    
  val tokensLeft = ref ([]: token list list)
                           



  (* 
   *   A SIMPLE PARSER FOR AN ML-LIKE SYNTAX
   * 
   *   decl ::= T_DEF T_SYM T_EQUAL expr  
   *            T_DEF T_SYM sym_list T_EQUAL expr
   *            expr
   *
   *   expr ::= eterm T_EQUAL eterm        
   *            eterm T_LESS eterm         
   *            eterm                      
   *
   *   eterm ::= cterm T_DCOLON cterm      
   *             cterm                     
   *
   *   cterm ::= term T_PLUS term         
   *             term T_MINUS term        
   *             term                     
   *
   *   term :: = aterm aterm_list        
   *            
   *   aterm_list ::= aterm aterm_list                [aterm_list_ATERM_LIST]
   *                  <empty>                         [aterm_list_EMPTY]
   *   
   *   aterm ::= T_INT                                      [aterm_INT]
   *             T_TRUE                                     [aterm_TRUE]
   *             T_FALSE                                    [aterm_FALSE]
   *             T_SYM                                      [aterm_SYM]
   *             T_BACKSLASH T_SYM T_RARROW expr            [aterm_FUN]
   *             T_LPAREN expr T_RPAREN                     [aterm_PARENS]
   *             T_IF expr T_THEN expr T_ELSE expr          [aterm_IF]
   *             T_LET T_SYM T_EQUAL expr T_IN expr         [aterm_LET]
   *             T_LET T_SYM T_SYM T_EQUAL expr T_IN expr   [aterm_LET_FUN]
   *             T_LBRACKET expr T_BAR T_SYM T_LARROW expr T_RBRACKET
   *             T_LBRACKET expr T_BAR T_SYM T_LARROW expr T_COMMA expr T_RBRACKET
   *)


  fun expect_INT ((T_INT i)::ts) = (soFar := (!soFar)@[T_INT i];SOME (i,ts))
    | expect_INT _ = NONE

  fun expect_SYM ((T_SYM s)::ts) = (soFar := (!soFar)@[T_SYM s];SOME (s,ts))
    | expect_SYM _ = NONE



  (*   expect tokens ts 
   *   checks if ts starts with tokens specified
   *   this can only be used for tokens that do not parse to a value 
   *)



  fun expect [] ts = SOME ts
    | expect (token::tokens) (t::ts) = if token = t then 
                                         (soFar := (!soFar)@[t];expect tokens ts)
                                       else NONE
    | expect _ _ = NONE


  fun expect_LET ts = expect [T_LET] ts
  fun expect_IN ts = expect [T_IN] ts
  fun expect_TRUE ts = expect [T_TRUE] ts
  fun expect_FALSE ts = expect [T_FALSE] ts
  fun expect_EQUAL ts = expect [T_EQUAL] ts
  fun expect_LESS ts = expect [T_LESS] ts
  fun expect_IF ts = expect [T_IF ] ts
  fun expect_THEN ts = expect [T_THEN] ts
  fun expect_ELSE ts = expect [T_ELSE] ts
  fun expect_LPAREN ts = expect [T_LPAREN ] ts
  fun expect_RPAREN ts = expect [T_RPAREN] ts
  fun expect_PLUS ts = expect [T_PLUS] ts
  fun expect_MINUS ts = expect [T_MINUS] ts
  fun expect_TIMES ts = expect [T_TIMES] ts
  fun expect_BACKSLASH ts = expect [T_BACKSLASH] ts
  fun expect_RARROW ts = expect [T_RARROW] ts
  fun expect_LARROW ts = expect [T_LARROW] ts
  fun expect_DCOLON ts = expect [T_DCOLON] ts
  fun expect_COMMA ts = expect [T_COMMA] ts
  fun expect_LBRACKET ts = expect [T_LBRACKET] ts
  fun expect_RBRACKET ts = expect [T_RBRACKET] ts
  fun expect_LBRACE ts = expect [T_LBRACE] ts
  fun expect_RBRACE ts = expect [T_RBRACE] ts
  fun expect_DOT ts = expect [T_DOT] ts
  fun expect_HASH ts = expect [T_HASH] ts
  fun expect_DDOTS ts = expect [T_DDOTS] ts
  fun expect_BAR ts = expect [T_BAR] ts
  fun expect_MATCH ts = expect [T_MATCH] ts
  fun expect_WITH ts = expect [T_WITH] ts
                          

  fun convertToString [] = ""
    | convertToString (t::ts) = (stringOfTokenEnglish t) ^ " " ^ (convertToString ts)


  fun findTokenBackwards [tk] [] = "... " ^(stringOfTokenEnglish tk)
    | findTokenBackwards [] ts = convertToString ts
    | findTokenBackwards tk (t::ts) = (case (expect tk (t::ts))
                                        of NONE =>
                                         (stringOfTokenEnglish t) ^ (findTokenBackwards tk ts)
                                | SOME ts => "")
    | findTokenBackwards _ _ = "ERROR: tokens are weird"

  fun findToken ([tk]::tokens) [] prev = " expected "^  "'" ^(stringOfTokenEnglish tk) ^ "'"
    | findToken (tk::tokens) (r::rest) prev = (case (expect tk (r::rest))
                                        of NONE =>
                                         findToken (tk::tokens) rest prev
                                | SOME ts => findTokenBackwards prev (r::rest))
    | findToken _ _ _ = "ERROR: tokens are weird"



    fun findSym [] = " expected symbol"
        | findSym (t::ts) = (case expect_SYM (t::ts)
                        of NONE => 
                                findSym ts
                        | SOME (s,ts) => (convertToString (t::ts)))
    fun findInt [] = " expected Int"
        |findInt (t::ts) = (case expect_INT (t::ts)
                        of NONE => 
                                findInt ts
                        | SOME (s,ts) => (convertToString (t::ts)))
    fun exprString ts = "<expr>"

    fun goThroughList [] = "empty \n"
        | goThroughList t = (convertToString t)

   fun updateSaved tokenList stVal need rest = (soFar := lexString ""; savedSoFar := (!savedSoFar)@[tokenList] ; stringVal := (!stringVal)@[stVal]; needToken := (!needToken)@[need]; tokensLeft := (!tokensLeft)@[rest])

   fun checkEType name = (if not(name = "expr") then
                                (if not(name = "field") then
                                        (if not(name = "term") then
                                                stringVal := ((!stringVal)@["<" ^ name ^ ">"]) else())else())else ()) 

   fun makeErrorLast [] [] = (soFar := lexString ""; bulkerr := lexString "";" ")
     | makeErrorLast (ts::tss) (e::es) = ((convertToString ts)  ^ e ^ " " ^ "\n" ^ (makeErrorLast tss es))
     | makeErrorLast _ (e::es) = ""
     | makeErrorLast _ _ = "huh?"

   fun makeErrorToken []          []      token      rest _ = (soFar := lexString ""; bulkerr := lexString ""; " ")
     | makeErrorToken (ts::tss) (e::es) (t::tokens)  (r::rest) previousToken = (if t = [] then
                ((convertToString ts)  ^ e ^ " " ^ "\n" ^ (makeErrorToken tss es tokens rest previousToken))
        else (if tokens = [] then
                ((convertToString ts)  ^ e ^ " " ^ (findToken [t] r previousToken) ^ "\n" ^ (makeErrorToken tss es tokens rest t))
        else
                ((convertToString ts)  ^ e ^ " " ^ (findToken (t::tokens) r previousToken) ^ " \n" ^ (makeErrorToken tss es tokens rest t))))
     | makeErrorToken _ (e::es) _ _ _=   ""
     | makeErrorToken _ _ _ _ _=  "huh?"

   fun makeErrorOther [] [] rest = (soFar := lexString ""; bulkerr := lexString ""; " ")
     | makeErrorOther (ts::tss) (e::es) rest = ((convertToString ts)  ^ e ^ " " ^ rest ^ "\n" ^ (makeErrorOther tss es rest))
     | makeErrorOther _ (e::es) _ =  ""
     | makeErrorOther _ _ _ =  "huh?"

   fun makeError funName errorName beforeTokens [[]] []       [[]] = (checkEType errorName;(err := "error in " ^ funName  ^ "- expected " ^ errorName  ^ "\n" ^ (makeErrorLast beforeTokens (!stringVal))))
     | makeError funName errorName beforeTokens left []       ts = (checkEType errorName; err := "error in " ^ funName  ^ "- expected " ^ errorName  ^ "\n" ^ (makeErrorToken beforeTokens (!stringVal) left ts []))
     | makeError funName errorName beforeTokens [[]] [sToken] [ts] = (checkEType errorName;(if sToken = "sym" then
        err := "error in " ^ funName  ^ "- expected " ^ errorName  ^ "\n" ^ (makeErrorOther beforeTokens (!stringVal) (findSym ts)) ^ (findSym ts)
        else if sToken = "int" then
        err := "error in " ^ funName  ^ "- expected " ^ errorName ^  "\n"  ^ (makeErrorOther beforeTokens (!stringVal) (findInt ts)) ^ (findInt ts)
        else if sToken = "expr" then
        err := "error in " ^ funName  ^ "- expected " ^ errorName ^  "\n"  ^ (makeErrorOther beforeTokens (!stringVal) (exprString ts)) ^ (exprString ts)
        else print "YOU ARE JUST WRONG"; soFar := lexString ""))
     | makeError _ _ _ _ _ _ = (err := "huh?" ; soFar := lexString "")

   fun makeError2 funName errorName [] beforeTokens input = err := "error in " ^ funName  ^ "- expected " ^ errorName ^  "\n"  ^ (convertToString beforeTokens) ^ "'" ^ input ^ "'"
     | makeError2 funName errorName ts beforeTokens  input= err := "error in " ^ funName  ^ "- expected " ^ errorName ^  "\n"  ^ (convertToString beforeTokens) ^ "'" ^ input ^ "'" ^ (convertToString ts)

  fun choose [] ts = NONE
    | choose (parser::parsers) ts = 
      (case parser ts
        of NONE => choose parsers ts
         | s => s)


  (*
   *  some helper functions to construct function calls in the internal representation
   *)

  fun call1 oper e1 = I.EApp (I.EIdent oper, e1)

  fun call2 oper e1 e2 = I.EApp (I.EApp (I.EIdent oper, e1), e2)


  fun parse_expr ts = 
      (case parse_eterm ts
        of NONE => NONE
         | SOME (e1,ts) => 
           (case expect_EQUAL ts
             of NONE => (case expect_LESS ts
                          of NONE => SOME (e1,ts)
                           | SOME ts => 
                             (case parse_eterm ts
                               of NONE => NONE
                                | SOME (e2,ts) => SOME (call2 "less" e1 e2, ts)))
              | SOME ts => 
                (case parse_eterm ts
                  of NONE => NONE
                   | SOME (e2,ts) => SOME (call2 "equal" e1 e2, ts))))
(* Question 3b *)
  and parse_fields ts =
    (case expect_SYM ts
      of NONE => SOME ([],ts)
      | SOME (s1,ts) =>
      (case expect_EQUAL ts
        of NONE => NONE
        | SOME ts =>
        (case parse_expr ts
          of NONE => NONE
          | SOME (e1,ts) =>
          (case expect_COMMA ts
            of NONE => SOME ([(s1,e1)],ts)
            | SOME ts =>
            (case parse_fields ts
              of NONE => NONE 
              | SOME (e2,ts) => SOME ((s1,e1)::e2,ts) )))))

(* Question 1a *)
  and parse_symList ts = 
        (case expect_SYM ts 
          of NONE => NONE
          | SOME (s1,ts) => 
          (case parse_symList ts 
           of NONE => SOME ([s1],ts)
           | SOME (ss,ts) => SOME (s1::ss,ts)))
           
           
  and parse_eterm ts = 
      (case parse_cterm ts
        of NONE => NONE
         | SOME (e1,ts) => 
           (case expect_DCOLON ts
             of NONE => SOME (e1,ts)
              | SOME ts => 
                ((updateSaved (!soFar) "<term>" [] []);(case parse_cterm ts
                  of NONE => ((makeError "Append List" "C term" (!savedSoFar) (!needToken) [] (!tokensLeft) ); NONE)
                   | SOME (e2,ts) => SOME (call2 "cons" e1 e2, ts)))))


  and parse_cterm ts = 
      (case parse_term ts
        of NONE => NONE
         | SOME (e1,ts) => 
           (case expect_PLUS ts
             of NONE =>
                (case expect_MINUS ts
                  of NONE => SOME (e1,ts)
                   | SOME ts => 
                     ((updateSaved (!soFar) "<term>" [] []);(case parse_term ts
                       of NONE => ((makeError "math" "term" (!savedSoFar) (!needToken) [] (!tokensLeft) ); NONE)
                        | SOME (e2,ts) => SOME (call2 "sub" e1 e2, ts))))
              | SOME ts => 
                ((updateSaved (!soFar) "<term>" [] []);(case parse_term ts
                  of NONE => ((makeError "math" "term" (!savedSoFar) (!needToken) [] (!tokensLeft)); NONE)
                   | SOME (e2,ts) => SOME (call2 "add" e1 e2, ts)))))


  and parse_term ts = let
    fun convert [] = parseError "empty list of aterms"
      | convert [at] = at
      | convert (at1::at2::ats) = convert ((I.EApp (at1,at2))::ats)
  in
    (case parse_aterm ts
       of NONE => NONE
        | SOME (at,ts) => 
          (case parse_aterm_list ts
             of NONE => NONE
              | SOME (ats,ts) => SOME (convert (at::ats),ts)))
  end
   


  and parse_aterm ts = 
      choose [parse_aterm_INT,
              parse_aterm_TRUE,
              parse_aterm_FALSE,
              parse_aterm_SYM,
              parse_aterm_FUN,
              parse_aterm_PARENS,
              parse_aterm_IF,
              parse_aterm_LET,
        parse_aterm_MATCH,
        parse_aterm_MAP,
        parse_aterm_RECORD,
        parse_aterm_FIELD
             ] ts

  and parse_aterm_INT ts = 
    (case expect_INT ts 
      of NONE => NONE
       | SOME (i,ts) => SOME (I.EVal (I.VInt i),ts))

  (* question 3b *)
  and parse_aterm_RECORD ts =
    (case expect_LBRACE ts
      of NONE => NONE
      | SOME ts => 
      ((updateSaved (!soFar) "<field>" [T_RBRACE] ts);(case parse_fields ts
        of NONE => ((makeError "record" "fields" (!savedSoFar) (!needToken)  [] (!tokensLeft) )  ; NONE)
        | SOME (recordList, ts) =>
        (case expect_RBRACE ts
          of NONE => ((makeError2 "record" "right brace" [] ((List.last(!savedSoFar))@(!soFar)) "}"); NONE)
          | SOME ts => SOME (I.ERecord recordList, ts)))))

  and parse_aterm_FIELD ts =
    (case expect_HASH ts
      of NONE => NONE
      | SOME ts =>
      (case expect_SYM ts
        of NONE => ((makeError2 "field" "sym" ts (!soFar) "sym") ; NONE)
        | SOME (s,ts) =>
        ((updateSaved (!soFar) "<expr>" [] []);(case parse_expr ts
          of NONE => ((makeError "field" "expr" (!savedSoFar) (!needToken) [] (!tokensLeft) ) ; NONE)
          | SOME (e,ts) => SOME (I.EField (e,s) , ts)))))

  and parse_aterm_TRUE ts = 
    (case expect_TRUE ts
      of NONE => NONE
       | SOME ts => SOME (I.EVal (I.VBool true),ts))

  and parse_aterm_FALSE ts = 
    (case expect_FALSE ts
      of NONE => NONE
       | SOME ts => SOME (I.EVal (I.VBool false),ts))

  and parse_aterm_SYM ts = 
    (case expect_SYM ts
      of NONE => NONE
       | SOME (s,ts) => SOME (I.EIdent s,ts))

  and parse_aterm_FUN ts = 
    (case expect_BACKSLASH ts 
      of NONE => NONE
       | SOME ts => 
         (case expect_SYM ts
           of NONE => ((makeError2 "function" "sym" ts (!soFar) "sym"); NONE)
            | SOME (s,ts) => 
              (case expect_RARROW ts
                of NONE => ((makeError2 "function" "right arrow" ts (!soFar) "->") ; NONE)
                 | SOME ts => 
                   ((updateSaved (!soFar) "<expr>" []);(case parse_expr ts
                     of NONE => ((makeError "function" "expr" (!savedSoFar) (!needToken) [] [[]] ) ; NONE)
                      | SOME (e,ts) => SOME (I.EFun (s,e), ts))))))

  and parse_aterm_PARENS ts = 
    (case expect_LPAREN ts
      of NONE => NONE
       | SOME ts =>
         ((updateSaved (!soFar) "<expr>" [T_RPAREN] ts);(case parse_expr ts
           of NONE => ((makeError "parentheses" "expr" (!savedSoFar) (!needToken)  [] (!tokensLeft) ) ; NONE)
            | SOME (e,ts) => 
              (case expect_RPAREN ts
                of NONE => ((makeError2 "parentheses" "right parentheses" [] ((List.last(!savedSoFar))@(!soFar)) ")") ; NONE)
                | SOME ts => SOME (e,ts)))))

  and parse_aterm_IF ts = 
    (case expect_IF ts
      of NONE => NONE
       | SOME ts => 
         ((updateSaved (!soFar) "<expr>" [T_THEN] ts);(case parse_expr ts
           of NONE => ((makeError "if" "expr" (!savedSoFar) (!needToken)  [] (!tokensLeft) ) ; NONE)
            | SOME (e1,ts) => 
              (case expect_THEN ts
                of NONE => ((makeError2 "if" "then" ts ((List.last(!savedSoFar))@(!soFar)) "then" ) ; NONE)
                 | SOME ts => 
                   ((updateSaved (!soFar) "<expr>" [T_ELSE] ts);(case parse_expr ts
                     of NONE => ((makeError "if" "expr" (!savedSoFar) (!needToken)  [] (!tokensLeft) ) ; NONE)
                      | SOME (e2,ts) => 
                        (case expect_ELSE ts
                          of NONE => ((makeError2 "if" "else" ts ((List.last(!savedSoFar))@(!soFar)) "else" ) ; NONE)
                           | SOME ts => 
                             ((updateSaved (!soFar) "<expr>" [] []);(case parse_expr ts
                               of NONE => ((makeError "if" "expr" (!savedSoFar) (!needToken) [] (!tokensLeft)); NONE)
                                | SOME (e3,ts) => SOME (I.EIf (e1,e2,e3),ts))))))))))

  and parse_aterm_LET ts = 
    (case expect_LET ts 
      of NONE => NONE
       | SOME ts => 
         (case expect_SYM ts 
           of NONE => ((makeError2 "let" "sym" ts (!soFar) "sym"); NONE)
            | SOME (s,ts) => 
              (case expect_EQUAL ts
                of NONE => (case parse_symList ts
                        of NONE => ((makeError2 "let" "decl" ts (!soFar) "decl"); NONE)
                        | SOME (nil,ts) => ((makeError2 "let" "sym List" ts (!soFar) "sym"); NONE)
                 | SOME ((param::ss),ts) => 
                   (case expect_EQUAL ts
                     of NONE => ((makeError2 "let" "equal" ts (!soFar) "="); NONE)
                      | SOME ts => 
                        ((updateSaved (!soFar) "<expr>" [T_IN] ts);(case parse_expr ts
                          of NONE => ((makeError "let" "expr" (!savedSoFar) (!needToken)  [] (!tokensLeft)); NONE)
                           |  SOME (e1,ts) => 
                             (case expect_IN ts
                               of NONE => ((makeError2 "let" "in" ts ((List.last(!savedSoFar))@(!soFar)) "in" ); NONE)
                                | SOME ts => 
                                  ((updateSaved (!soFar) "<expr>" [] []);(case parse_expr ts
                                    of NONE => ((makeError "let" "expr" (!savedSoFar) (!needToken) [] (!tokensLeft) ); NONE)
                                    | SOME (e2,ts) => let 
                                        fun paramFun (paramS::nil) = I.EFun (paramS,e1)
                                          | paramFun (paramS::ss) = I.EFun (paramS,paramFun ss)
                                          | paramFun _ = e1
                                        in
                                         SOME (I.ELetFun (s,param,paramFun ss,e2),ts)
                                        end)))))))
                 | SOME ts => 
                   ((updateSaved (!soFar) "<expr>" [T_IN] ts);(case parse_expr ts
                     of NONE => ((makeError "let" "expr" (!savedSoFar) (!needToken)  [] (!tokensLeft) ); NONE)
                      | SOME (e1,ts) => 
                        (case expect_IN ts
                          of NONE => ((makeError2 "let" "in" ts ((List.last(!savedSoFar))@(!soFar)) "in") ; NONE)
                           | SOME ts => 
                             ((updateSaved (!soFar) "<expr>" [] []);(case parse_expr ts
                               of NONE => ((makeError "let" "expr" (!savedSoFar) (!needToken) [] (!tokensLeft)) ; NONE)
                                | SOME (e2,ts) => SOME (I.ELet (s,e1,e2),ts)))))))))

  and parse_expr_list ts = 
    (case parse_expr ts
      of NONE => NONE
       | SOME (e,ts) => 
         (case expect_COMMA ts
            of NONE => SOME (call2 "cons" e  (I.EVal (I.VList [])), ts)
             | SOME ts =>
                ((updateSaved (!soFar) "<expr>" [] []); (case parse_expr_list ts
                 of NONE => ((makeError2 "expr list" "expr" [] (!soFar) "expr"); NONE)
                  | SOME (es, ts) => SOME (call2 "cons" e es, ts)))))

  and parse_aterm_MAP ts = 
    (case expect_LBRACKET ts
      of NONE => NONE
      | SOME ts =>
        (case parse_expr_list ts
          of NONE => 
            (case expect_RBRACKET ts
              of NONE => ((makeError2 "list" "list entries or right bracket" [] (!soFar) "]") ; NONE)
              | SOME ts => SOME (I.EVal (I.VNil), ts))
          | SOME (es, ts) =>
            (case expect_RBRACKET ts
              of NONE => (case expect_DDOTS ts
                of NONE => (case expect_BAR ts
                  of NONE => ((makeError2 "map or filter" "bar" ts (!soFar) "|") ; NONE)
                  | SOME ts =>
                    (case expect_SYM ts
                      of NONE => ((makeError2 "map or filter" "sym" ts (!soFar) "sym") ; NONE)
                      | SOME (s,ts) =>
                        (case expect_LARROW ts
                          of NONE => ((makeError2 "map or filter" "left arrow" ts (!soFar) "<-") ; NONE)
                          | SOME ts =>
                            ((updateSaved (!soFar) "<expr>" [T_RBRACKET] ts);(case parse_expr ts
                              of NONE => ((makeError "map or filter" "expr" (!savedSoFar) (!needToken)  [] (!tokensLeft)) ; NONE)
                              | SOME (e2, ts) =>
                                (case expect_RBRACKET ts
                                  of NONE =>
                                    (case expect_COMMA ts
                                      of NONE => ((makeError2 "map or filter" "right bracket or comma" ts ((List.last(!savedSoFar))@(!soFar)) "]") ; NONE)
                                      | SOME ts =>
                                        ((updateSaved (!soFar) "<expr>" [T_RBRACKET] ts);(case parse_expr ts
                                          of NONE => ((makeError "filter" "expr" (!savedSoFar) (!needToken) [] (!tokensLeft) ) ; NONE)
                                          | SOME (e3, ts) =>
                                            (case expect_RBRACKET ts
                                              of NONE => ((makeError2 "filter" "right bracket" [] ((List.last(!savedSoFar))@(!soFar)) "]") ; NONE)
                                              | SOME ts => let
                                                val x = (call2 "filter" (I.EFun(s,e3)) e2)
                                              in
                                                SOME (call2 "map" (I.EFun(s, (call1 "hd" es))) x, ts)
                                              end))))
                                  | SOME ts => SOME ((call2 "map" (I.EFun(s, (call1 "hd" es))) e2), ts)))))))
                | SOME ts =>
                  ((updateSaved (!soFar) "<expr>" [T_RBRACKET] ts);(case parse_expr ts
                    of NONE => ((makeError "interval" "expr" (!savedSoFar) (!needToken)  [] (!tokensLeft) ) ; NONE)
                    | SOME (e2, ts) =>
                      (case expect_RBRACKET ts
                        of NONE => ((makeError2 "interval" "right bracket" [] ((List.last(!savedSoFar))@(!soFar)) "]"); NONE)
                        | SOME ts => SOME (call2 "interval" (call1 "hd" es) e2, ts)))))
              | SOME ts => SOME (es, ts))))
  
  and parse_aterm_list ts = 
      choose [parse_aterm_list_ATERM_LIST,
              parse_aterm_list_EMPTY
             ] ts 

  and parse_aterm_list_ATERM_LIST ts = 
    (case parse_aterm ts
      of NONE => NONE
       | SOME (at,ts) => 
         (case parse_aterm_list ts
           of NONE => NONE
            | SOME (ats,ts) => SOME (at::ats,ts)))

  and parse_aterm_list_EMPTY ts = SOME ([], ts)


  (*Question 2d*)
  and parse_aterm_MATCH ts =
    (case expect_MATCH ts
      of NONE => NONE
       | SOME ts =>
         ((updateSaved (!soFar) "<expr>" [T_WITH] ts);(case parse_expr ts
            of NONE => ((makeError "match" "expr" (!savedSoFar) (!needToken)  [] (!tokensLeft) ); NONE)
             | SOME (e1,ts) =>
               (case expect_WITH ts
                of NONE => ((makeError2 "match" "with" ts ((List.last(!savedSoFar))@(!soFar)) "with"); NONE)
                 | SOME ts =>
                   (case expect_LBRACKET ts
                      of NONE => ((makeError2 "match" "left bracket" ts ((List.last(!savedSoFar))@(!soFar)) "["); NONE)
                       | SOME ts =>
                         (case expect_RBRACKET ts
                            of NONE => ((makeError2 "match" "right bracket" ts ((List.last(!savedSoFar))@(!soFar))"]" ); NONE)
                             | SOME ts =>
                               (case expect_RARROW ts
                                  of NONE => ((makeError2 "match" "right arrow" ts ((List.last(!savedSoFar))@(!soFar)) "->"); NONE)
                                   | SOME ts =>
                                     ((updateSaved (!soFar) "<expr>" [T_BAR] ts);(case parse_expr ts
                                      of NONE => ((makeError "match" "expr" (!savedSoFar) (!needToken) [] (!tokensLeft) ); NONE)
                                       | SOME (e2, ts) =>
                                         (case expect_BAR ts
                                            of NONE => ((makeError2 "match" "bar" ts ((List.last(!savedSoFar))@(!soFar)) "|"); NONE)
                                             | SOME ts =>
                                               (case expect_SYM ts
                                                  of NONE => ((makeError2 "match" "sym" ts ((List.last(!savedSoFar))@(!soFar)) "sym"); NONE)
                                                   | SOME (sym1,ts) =>
                                                     (case expect_DCOLON ts
                                                        of NONE => ((makeError2 "match" "double colon" ts ((List.last(!savedSoFar))@(!soFar)) "::"); NONE)
                                                         | SOME ts =>
                                                           (case expect_SYM ts
                                                              of NONE => ((makeError2 "match" "sym" ts ((List.last(!savedSoFar))@(!soFar)) "sym"); NONE)
                                                               | SOME (sym2,ts) =>
                                                                 (case expect_RARROW ts
                                                                    of NONE => ((makeError2 "match" "right arrow" ts ((List.last(!savedSoFar))@(!soFar)) "->"); NONE)
                                                                     | SOME ts =>
                                                                       ((updateSaved (!soFar) "<expr>" []);(case parse_expr ts
                                                                          of NONE => ((makeError "match" "expr" (!savedSoFar) [[]] [] [[]] ); NONE)
                                                                           | SOME (e3, ts) =>

                                                                           let
                                                                             val s1 =  (call1 "hd" e1)
                                                                             val s2 =  (call1 "tl" e1)
                                                                            in
                                                                              SOME (I.EIf ((call2 "equal" e1 (I.EVal (I.VNil))) , e2 , I.ELet( sym1, s1 , I.ELet(sym2, s2, e3))), ts)
                                                                            end

                                                                           ))))))))))))))))
  
  datatype decl = DDef of string * I.expr
                | DExpr of I.expr
                | DSpace

     
 fun parse_decl ts = let
   fun decl_val ts = 
    (case expect [T_DEF] ts
      of NONE => NONE
       | SOME ts => 
         (case expect_SYM ts
           of NONE => ((makeError2 "def" "sym" ts (!soFar) "sym"); NONE)
          | SOME (s,ts) =>
            (case expect [T_EQUAL] ts
              of NONE => ((makeError2 "def" "equal" ts (!soFar) "="); NONE)
              | SOME ts => 
                (case parse_expr ts
                  of NONE => ((makeError2 "def" "expr" [] (!soFar) "expr"); NONE)
                  | SOME (e,ts) => SOME (DDef (s,e),ts)))))
   fun decl_fun ts = 
     (case expect [T_DEF] ts
       of NONE => NONE
       | SOME ts =>
         (case expect_SYM ts
           of NONE => ((makeError2 "def" "sym" ts (!soFar) "sym"); NONE)
           | SOME (s,ts) => 
             (case expect_SYM ts
               of NONE => ((makeError2 "def" "sym" ts (!soFar) "sym"); NONE)
               | SOME (param,ts) => 
                 (case expect [T_EQUAL] ts
                   of NONE => ((makeError2 "def" "equal" ts (!soFar) "="); NONE)
                   | SOME ts => 
                     (case parse_expr ts 
                       of NONE => ((makeError2 "def" "expr" [] (!soFar) "expr"); NONE)
                       | SOME (e,ts) => 
                          SOME (DDef (s,I.ELetFun (s,param,e,(I.EIdent s))),ts))))))
   fun decl_expr ts = 
     (case parse_expr ts
       of NONE => NONE
       | SOME (e,ts) => SOME (DExpr e, ts))
  in
    choose [decl_val, decl_fun, decl_expr] ts
  end


  fun parseExpr ts = 
      (err := "unknown error"; (case parse_expr ts
        of SOME (e,[]) => e
         | SOME (_,_)  => parseError "expression Failure"
         | NONE => parseError (!err)))

  fun parseDecl [] = DSpace
    | parseDecl ts = 
      (err := "unknown error"; soFar := lexString  "";savedSoFar := ([]: token list list); needToken := ([]: token list list);tokensLeft := ([]: token list list); stringVal := []; bulkerr := lexString "";(case parse_decl ts

        of SOME (d,[]) => d
         | SOME (_,left)  => (funErr := "Function Call Failed \n";(parseDecl left))
         | NONE => parseError (!err ^ (!funErr))))
      
end
