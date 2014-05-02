(* 
 *   Code for project. modified from homework 4 and lecture 10
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

(*Globals used for keeping track of error messages*)
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
                 | T_true 
                 | T_false
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
    | stringOfToken T_true = "T_true"
    | stringOfToken T_false = "T_false"
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

(*translate tokens to symbols used in our language to spit back at the user*)
      fun stringOfTokenEnglish T_LET = "let"
    | stringOfTokenEnglish T_IN = "in"
    | stringOfTokenEnglish (T_SYM s) = s
    | stringOfTokenEnglish (T_INT i) = Int.toString i
    | stringOfTokenEnglish T_true = "true"
    | stringOfTokenEnglish T_false = "false"
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
    | produceSymbol "true" = SOME (T_true)
    | produceSymbol "false" = SOME (T_false)
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
                      
   
(*global that keeps track of current layer of tokens*)
  val soFar = ref ([]: token list)
(*global that keeps track of every layer of passed tokens*)
  val savedSoFar = ref ([]: token list list)
(*global that keeps track of the first token on the "following" side for each layer*)
  val needToken =  ref ([]: token list list)
(*global that keeps track of the missing part for each layer*)  
  val stringVal = ref ([]: string list)   
(*global that keeps track of all following tokens for each layer*)
  val tokensLeft = ref ([]: token list list)
(*global used to make sure "single token" errors are not over written by nesting*)
  val singleError = ref false
(*global used for when multiple of the same parse trees are present in the same function*)
  val hold =  ref ([]: token list)
  (*global used to keep track of when the messging "skips over" an internal parse tree of the same type
            Example
                  let y = (let x = 10 + in x + x) in y + y
                  a "skip" would occur over the  "in x + x" to the "in y + y"*)
  val skipped = ref false



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
   *             T_true                                     [aterm_true]
   *             T_false                                    [aterm_false]
   *             T_SYM                                      [aterm_SYM]
   *             T_BACKSLASH T_SYM T_RARROW expr            [aterm_FUN]
   *             T_LPAREN expr T_RPAREN                     [aterm_PARENS]
   *             T_IF expr T_THEN expr T_ELSE expr          [aterm_IF]
   *             T_LET T_SYM T_EQUAL expr T_IN expr         [aterm_LET]
   *             T_LET T_SYM T_SYM T_EQUAL expr T_IN expr   [aterm_LET_FUN]
   *             T_LBRACKET expr T_BAR T_SYM T_LARROW expr T_RBRACKET
   *             T_LBRACKET expr T_BAR T_SYM T_LARROW expr T_COMMA expr T_RBRACKET
   *)

(*changed expect functions so that they track the tokens that have passed using the "soFar" global*)
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
  fun expect_true ts = expect [T_true] ts
  fun expect_false ts = expect [T_false] ts
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
                          
(*function used to convert a list of tokens to a "readable" output*)
  fun convertToString [] = ""
    | convertToString (t::ts) = (stringOfTokenEnglish t) ^ " " ^ (convertToString ts)

(*used to check what the next token in the list is. used for list error message generation*)
  fun nextToken token [] = false
    | nextToken token (r::rest) = if r = token then true
                                        else false

(*used to check if a token string contains a token. 
    Used to see if there are nested forms of the same tree for end error generation. 
      Parameters:
            token = token, the token that we are trying to find
            rest = token list, what we are looking through
            stuff = toke list, a saved version of this instance of the token. Used later to generate 
                        error messaging when we need the last copy
      Example
            let y = (let x = 10 + in x + x) in y + y
             there are two let trees here*)
  fun hasToken token [] stuff= false
    | hasToken token ([]::rest) stuff = (hasToken token rest stuff)
    | hasToken token ([r]::rest) stuff = if token = r then (skipped := true; hold := stuff;true)
                                        else (hasToken token rest stuff)
    | hasToken _ _ _ = (print "Internal Error: we done messed up at hasToken"; false)


(*findToken helper function:
              finds the trailing end of each layer to use as a cut off point
              parameters:
                  [tk] = token list, the token we are trying to end at
                  (t::ts) = token list, all the tokens found after the beginning of the end
              Example:
                  let x = (10 + (3 +)) in x + x
                  findTokenBackwards for the layer (10 + ) looks for the "in" token *)
  fun findTokenBackwards [tk] [] = "... " ^(stringOfTokenEnglish tk)
    | findTokenBackwards [] ts = convertToString ts
    | findTokenBackwards [tk] (t::ts) =  if (tk = t) then  ""
                                      else
                                         (stringOfTokenEnglish t) ^ " "^ (findTokenBackwards [tk] ts)
    | findTokenBackwards _ _ = (print "Internal Error: we done messed up at findTokenBackwards"; "")

(*Function used to start the trailing end of the error message at each layer
    uses above functions to look for duplecates, jump to the correct layer, and know when each layer stops
              parameters:
                  ([tk]::tokens) = token list list, this contains the beginning of the end for each later
                                    where tk is the beginning of the end for the current layer
                  (r::rest) = token list, contains all of the tokens left in the current layer (after error)  *)
  fun findToken ([tk]::tokens) [] prev = (if (!skipped) then
                                                  (findTokenBackwards prev (!hold)) 
                                                else 
                                                  " expected "^  "'" ^(stringOfTokenEnglish tk) ^ "'")
    | findToken ([tk]::tokens) (r::rest) prev= if (tk = r) then
                                          (if (hasToken tk tokens (r::rest)) then
                                            findToken ([tk]::tokens) rest prev
                                          else
                                        (stringOfTokenEnglish r) ^" "^ (findTokenBackwards prev rest))
                                      else
                                         findToken ([tk]::tokens) rest prev
    | findToken _ _ _ = "ERROR: tokens are weid"

(*helper function for makeErrorNested 
        is used when the error in question occured at the end of the token list rather then somewhere in the middle
          Parameters:
              (ts::tss)  = token list list, each layers tokens leading up to the error
               (e::es) = string list, the official error name at each layer  *)
   fun makeErrorLast [] [] = (soFar := lexString "";" ")
     | makeErrorLast (ts::tss) (e::es) = ((convertToString ts)  ^ e ^ " " ^ "\n" ^ (makeErrorLast tss es))
     | makeErrorLast _ (e::es) = ""
     | makeErrorLast _ _ = "huh?"

(*helper function for makeError
        Used when the error is not at the end of the token list
          parameters:
              (ts::tss) = token list list, contains all tokens leading up to the error for each layer
              (e::es) = string list, contains the official error name at each layer
              (t::tokens) = token list list, contains the beginning of the end for each of the layers
              (r::rest) = token list list, contains all tokens that follow the error for each of the layers
              previousToken  = token, contains the beginning of the end for the previous layer
                            necessary for finding the end of the end for the current layer.  *)
   fun makeErrorToken []          []      token      rest _ = (soFar := lexString ""; " ")
     | makeErrorToken (ts::tss) (e::es) (t::tokens)  (r::rest) previousToken = (if t = [] then
                ((convertToString ts)  ^ e ^ " " ^ "\n" ^ (makeErrorToken tss es tokens rest previousToken))
          else (if tokens = [] then
                ((convertToString ts)  ^ e ^ " " ^ (findToken [t] r previousToken) ^ "\n" ^ (makeErrorToken tss es tokens rest t))
        else
                ((convertToString ts)  ^ e ^ " " ^ (findToken (t::tokens) r previousToken) ^ " \n" ^ (makeErrorToken tss es tokens rest t))))
     | makeErrorToken _ (e::es) _ _ _=   ""
     | makeErrorToken _ _ _ _ _=  "huh?"

(* function for making error Messages of nested functions
          Parameters: 
                funName = string, name of the primary expression that failed
                errorName = string, way in which the primarly expression failed
                beforeTokens = token list list, contains all tokens leading up to the error for each layer
                left = token list list, contains the beginning of the end for each of the layers
                ts = token list list, contains all tokens that follow the error for each of the layers*)
   fun makeErrorNested funName errorName beforeTokens [[]] [[]] = (if (!singleError) = false then 
                        err := "error in " ^ funName  ^ "- expected " ^ errorName  ^ "\n" ^ (makeErrorLast beforeTokens (!stringVal))
                        else
                        err := !err ^ "")
     | makeErrorNested funName errorName beforeTokens left  ts = (if (!singleError) = false then 
                        err := "error in " ^ funName  ^ "- expected " ^ errorName  ^ "\n" ^ (makeErrorToken beforeTokens (!stringVal) left ts [])
                        else 
                        err := !err ^ "")
(*Function used to flatten a list of lists
          used for flattening all nested layers
          needed for making error messages for none nested functions*)
   fun pealSaved [] = []
     | pealSaved (layer::layers) = layer@(pealSaved(layers))
(*function for making error messages of non-nested functions
            Parameters:
                funName = string, name of the primary expression that failed
                errorName = string, way in which the primarly expression failed
                ts = token list, contains all tokens that follow the error
                beforeTokens = token list list, contains all tokens leading up to the error for each layer
                input = string, version of the errorName that is syntactically correct*)
   fun makeError2 funName errorName [] beforeTokens input = (singleError := true ;err := "error in " ^ funName  ^ "- expected " ^ errorName ^  "\n"  ^ (convertToString ((pealSaved (!savedSoFar))@beforeTokens)) ^ "'" ^ input ^ "'")
     | makeError2 funName errorName ts beforeTokens  input= (singleError := true ;err := "error in " ^ funName  ^ "- expected " ^ errorName ^  "\n"  ^ (convertToString ((pealSaved (!savedSoFar))@beforeTokens)) ^ "'" ^ input ^ "'" ^ (convertToString ts))

(*Used to save each layer of a given line as a separate entity. 
    clears temperary storage used for keeping track of the current layer
          Parameters: 
            stVal = string, the current layers error message
            need = token list, the current layers token that indicates the beginning of the end
            rest = token list, the tokens that follow the error on the current list*)
   fun updateSaved stVal need rest = (savedSoFar := (!savedSoFar)@[(!soFar)] ; soFar := lexString ""; stringVal := (!stringVal)@[stVal]; needToken := (!needToken)@[need]; tokensLeft := (!tokensLeft)@[rest]) 
 
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
                ((updateSaved "<term>" [] []);(case parse_cterm ts
                  of NONE => ((makeErrorNested "Append List" "C term" (!savedSoFar) (!needToken) (!tokensLeft) ); NONE)
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
                     ((updateSaved "<term>" [] []);(case parse_term ts
                       of NONE => ((makeErrorNested "math" "term" (!savedSoFar) (!needToken) (!tokensLeft) ); NONE)
                        | SOME (e2,ts) => SOME (call2 "sub" e1 e2, ts))))
              | SOME ts => 
                ((updateSaved "<term>" [] []);(case parse_term ts
                  of NONE => ((makeErrorNested "math" "term" (!savedSoFar) (!needToken) (!tokensLeft)); NONE)
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
              parse_aterm_true,
              parse_aterm_false,
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


  and parse_aterm_RECORD ts =
    (case expect_LBRACE ts
      of NONE => NONE
      | SOME ts => 
      ((updateSaved "<field>" [T_RBRACE] ts);(case parse_fields ts
        of NONE => ((makeErrorNested "record" "fields" (!savedSoFar) (!needToken) (!tokensLeft) )  ; NONE)
        | SOME (recordList, ts) =>
        (case expect_RBRACE ts
          of NONE => ((makeError2 "record" "right brace" [] (!soFar) "}"); NONE)
          | SOME ts => SOME (I.ERecord recordList, ts)))))

  and parse_aterm_FIELD ts =
    (case expect_HASH ts
      of NONE => NONE
      | SOME ts =>
      (case expect_SYM ts
        of NONE => ((makeError2 "field" "sym" ts (!soFar) "sym") ; NONE)
        | SOME (s,ts) =>
        ((updateSaved "<expr>" [] []);(case parse_expr ts
          of NONE => ((makeErrorNested "field" "expr" (!savedSoFar) (!needToken) (!tokensLeft) ) ; NONE)
          | SOME (e,ts) => SOME (I.EField (e,s) , ts)))))

  and parse_aterm_true ts = 
    (case expect_true ts
      of NONE => NONE
       | SOME ts => SOME (I.EVal (I.VBool true),ts))

  and parse_aterm_false ts = 
    (case expect_false ts
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
                   ((updateSaved "<expr>" []);(case parse_expr ts
                     of NONE => ((makeErrorNested "function" "expr" (!savedSoFar) (!needToken) [[]] ) ; NONE)
                      | SOME (e,ts) => SOME (I.EFun (s,e), ts))))))

  and parse_aterm_PARENS ts = 
    (case expect_LPAREN ts
      of NONE => NONE
       | SOME ts =>
         ((updateSaved "<expr>" [T_RPAREN] ts);(case parse_expr ts
           of NONE => ((makeErrorNested "parentheses" "expr" (!savedSoFar) (!needToken) (!tokensLeft) ) ; NONE)
            | SOME (e,ts) => 
              (case expect_RPAREN ts
                of NONE => ((makeError2 "parentheses" "right parentheses" [] (!soFar) ")") ; NONE)
                | SOME ts => SOME (e,ts)))))

  and parse_aterm_IF ts = 
    (case expect_IF ts
      of NONE => NONE
       | SOME ts => 
         ((updateSaved "<expr>" [T_THEN] ts);(case parse_expr ts
           of NONE => ((makeErrorNested "if" "expr" (!savedSoFar) (!needToken) (!tokensLeft) ) ; NONE)
            | SOME (e1,ts) => 
              (case expect_THEN ts
                of NONE => ((makeError2 "if" "then" ts (!soFar) "then" ) ; NONE)
                 | SOME ts => 
                   ((updateSaved "<expr>" [T_ELSE] ts);(case parse_expr ts
                     of NONE => ((makeErrorNested "if" "expr" (!savedSoFar) (!needToken) (!tokensLeft) ) ; NONE)
                      | SOME (e2,ts) => 
                        (case expect_ELSE ts
                          of NONE => ((makeError2 "if" "else" ts (!soFar) "else" ) ; NONE)
                           | SOME ts => 
                             ((updateSaved "<expr>" [] []);(case parse_expr ts
                               of NONE => ((makeErrorNested "if" "expr" (!savedSoFar) (!needToken) (!tokensLeft)); NONE)
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
                               ((updateSaved "<expr>" [T_IN] ts);(case parse_expr ts
                                 of NONE => ((makeErrorNested "let" "expr" (!savedSoFar) (!needToken) (!tokensLeft)); NONE)
                                  |  SOME (e1,ts) => 
                                    (case expect_IN ts
                                      of NONE => ((makeError2 "let" "in" ts (!soFar) "in" ); NONE)
                                       | SOME ts => 
                                         ((updateSaved "<expr>" [] []);(case parse_expr ts
                                           of NONE => ((makeErrorNested "let" "expr" (!savedSoFar) (!needToken) (!tokensLeft) ); NONE)
                                           | SOME (e2,ts) => let 
                                               fun paramFun (paramS::nil) = I.EFun (paramS,e1)
                                                 | paramFun (paramS::ss) = I.EFun (paramS,paramFun ss)
                                                 | paramFun _ = e1
                                               in
                                                SOME (I.ELetFun (s,param,paramFun ss,e2),ts)
                                               end)))))))
                 | SOME ts => 
                   ((updateSaved "<expr>" [T_IN] ts);(case parse_expr ts
                     of NONE => ((makeErrorNested "let" "expr" (!savedSoFar) (!needToken) (!tokensLeft) ); NONE)
                      | SOME (e1,ts) => 
                        (case expect_IN ts
                          of NONE => ((makeError2 "let" "in" ts (!soFar) "in") ; NONE)
                           | SOME ts => 
                             ((updateSaved "<expr>" [] []);(case parse_expr ts
                               of NONE => ((makeErrorNested "let" "expr" (!savedSoFar) (!needToken) (!tokensLeft)) ; NONE)
                                | SOME (e2,ts) => SOME (I.ELet (s,e1,e2),ts)))))))))
(*changed parse experesion list so it can distinguish between a list or a filter when there is an error near its beginning*)
  and parse_expr_list ts = 
    (case parse_expr ts
      of NONE => (if (nextToken T_RBRACKET ts) then
                        ((makeError2 "list" "expr" ts (!soFar) "expr");NONE)
                else if (nextToken T_COMMA ts) then
                        ((makeError2 "list" "expr" ts (!soFar) "expr");NONE)
                else if (nextToken T_DCOLON ts) then
                        ((makeError2 "interval" "expr" ts (!soFar) "expr");NONE)
                else                        
                        NONE)
       | SOME (e,ts) => 
         (case expect_COMMA ts
            of NONE => SOME (call2 "cons" e  (I.EVal (I.VList [])), ts)
             | SOME ts =>
                (case parse_expr_list ts
                 of NONE => NONE
                  | SOME (es, ts) => SOME (call2 "cons" e es, ts))))

(*combined map, interval, filter, list*)
  and parse_aterm_MAP ts = 
    (case expect_LBRACKET ts
      of NONE => NONE
      | SOME ts =>
        (case parse_expr_list ts
          of NONE => 
            (case expect_RBRACKET ts
              of NONE => NONE
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
                            ((updateSaved "<expr>" [T_RBRACKET] ts);(case parse_expr ts
                              of NONE => ((makeErrorNested "map or filter" "expr" (!savedSoFar) (!needToken) (!tokensLeft)) ; NONE)
                              | SOME (e2, ts) =>
                                (case expect_RBRACKET ts
                                  of NONE =>
                                    (case expect_COMMA ts
                                      of NONE => ((makeError2 "map or filter" "right bracket or comma" ts (!soFar) "]") ; NONE)
                                      | SOME ts =>
                                        ((updateSaved "<expr>" [T_RBRACKET] ts);(case parse_expr ts
                                          of NONE => ((makeErrorNested "filter" "expr" (!savedSoFar) (!needToken) (!tokensLeft) ) ; NONE)
                                          | SOME (e3, ts) =>
                                            (case expect_RBRACKET ts
                                              of NONE => ((makeError2 "filter" "right bracket" [] (!soFar) "]") ; NONE)
                                              | SOME ts => let
                                                val x = (call2 "filter" (I.EFun(s,e3)) e2)
                                              in
                                                SOME (call2 "map" (I.EFun(s, (call1 "hd" es))) x, ts)
                                              end))))
                                  | SOME ts => SOME ((call2 "map" (I.EFun(s, (call1 "hd" es))) e2), ts)))))))
                | SOME ts =>
                  ((updateSaved "<expr>" [T_RBRACKET] ts);(case parse_expr ts
                    of NONE => ((makeErrorNested "interval" "expr" (!savedSoFar) (!needToken) (!tokensLeft) ) ; NONE)
                    | SOME (e2, ts) =>
                      (case expect_RBRACKET ts
                        of NONE => ((makeError2 "interval" "right bracket" [] (!soFar) "]"); NONE)
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

  and parse_aterm_MATCH ts =
    (case expect_MATCH ts
      of NONE => NONE
       | SOME ts =>
         ((updateSaved "<expr>" [T_WITH] ts);(case parse_expr ts
            of NONE => ((makeErrorNested "match" "expr" (!savedSoFar) (!needToken) (!tokensLeft) ); NONE)
             | SOME (e1,ts) =>
               (case expect_WITH ts
                of NONE => ((makeError2 "match" "with" ts (!soFar) "with"); NONE)
                 | SOME ts =>
                   (case expect_LBRACKET ts
                      of NONE => ((makeError2 "match" "left bracket" ts (!soFar) "["); NONE)
                       | SOME ts =>
                         (case expect_RBRACKET ts
                            of NONE => ((makeError2 "match" "right bracket" ts (!soFar)"]" ); NONE)
                             | SOME ts =>
                               (case expect_RARROW ts
                                  of NONE => ((makeError2 "match" "right arrow" ts (!soFar) "->"); NONE)
                                   | SOME ts =>
                                     ((updateSaved "<expr>" [T_BAR] ts);(case parse_expr ts
                                      of NONE => ((makeErrorNested "match" "expr" (!savedSoFar) (!needToken) (!tokensLeft) ); NONE)
                                       | SOME (e2, ts) =>
                                         (case expect_BAR ts
                                            of NONE => ((makeError2 "match" "bar" ts (!soFar) "|"); NONE)
                                             | SOME ts =>
                                               (case expect_SYM ts
                                                  of NONE => ((makeError2 "match" "sym" ts (!soFar) "sym"); NONE)
                                                   | SOME (sym1,ts) =>
                                                     (case expect_DCOLON ts
                                                        of NONE => ((makeError2 "match" "double colon" ts (!soFar) "::"); NONE)
                                                         | SOME ts =>
                                                           (case expect_SYM ts
                                                              of NONE => ((makeError2 "match" "sym" ts (!soFar) "sym"); NONE)
                                                               | SOME (sym2,ts) =>
                                                                 (case expect_RARROW ts
                                                                    of NONE => ((makeError2 "match" "right arrow" ts (!soFar) "->"); NONE)
                                                                     | SOME ts =>
                                                                       ((updateSaved "<expr>" []);(case parse_expr ts
                                                                          of NONE => ((makeErrorNested "match" "expr" (!savedSoFar) [[]] [[]] ); NONE)
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
   fun decl_val_fun ts = 
    (case expect [T_DEF] ts
      of NONE => NONE
       | SOME ts => 
         (case expect_SYM ts
           of NONE => ((makeError2 "def" "sym" ts (!soFar) "sym"); NONE)
          | SOME (s,ts) =>
            (case expect [T_EQUAL] ts
              of NONE => 
               (case expect_SYM ts
                 of SOME (param,ts) => 
                  (case expect [T_EQUAL] ts
                    of NONE => ((makeError2 "def" "equal" ts (!soFar) "="); NONE)
                    | SOME ts => 
                      (case parse_expr ts 
                        of NONE => ((makeError2 "def" "expr" [] (!soFar) "expr"); NONE)
                        | SOME (e,ts) => 
                           SOME (DDef (s,I.ELetFun (s,param,e,(I.EIdent s))),ts)))
                 | NONE => ((makeError2 "def" "equal" ts (!soFar) "="); NONE))
              | SOME ts => 
                (case parse_expr ts
                  of NONE => ((makeError2 "def" "expr" [] (!soFar) "expr"); NONE)
                  | SOME (e,ts) => SOME (DDef (s,e),ts)))))
   fun decl_expr ts = 
     (case parse_expr ts
       of NONE => NONE
       | SOME (e,ts) => SOME (DExpr e, ts))
  in
    choose [decl_val_fun, decl_expr] ts
  end


  fun parseExpr ts = 
      (err := "unknown error"; (case parse_expr ts
        of SOME (e,[]) => e
         | SOME (_,_)  => parseError "expression Failure"
         | NONE => parseError (!err)))

  fun parseDecl [] = DSpace
    | parseDecl ts = 
      (err := "unknown error"; soFar := lexString  "";skipped := false; hold := ([]: token list);savedSoFar := ([]: token list list); singleError := false; needToken := ([]: token list list);tokensLeft := ([]: token list list); stringVal := []; (case parse_decl ts

        of SOME (d,[]) => d
         | SOME (_,left)  => (funErr := "Function Call Failed \n";(parseDecl left))
         | NONE => parseError (!err ^ (!funErr))))
      
end
