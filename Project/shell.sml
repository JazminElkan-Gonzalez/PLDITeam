structure Shell = struct

  structure P = Parser
  structure I = InternalRepresentation
  structure E = Evaluator



  (*   Specialized function to force values for printing in the shell
   *   but only force up to d elements of lists
   *)

  fun stringOfValue v d = stringOfValue' (E.force v) d

  and stringOfValue' (I.VInt i) d = Int.toString i
    | stringOfValue' (I.VBool true) d = "true"
    | stringOfValue' (I.VBool false) d = "false"
    | stringOfValue' (I.VClosure (n,e,_)) d = 
        String.concat ["<function (", n, ",", I.stringOfExpr e,")>"]
    | stringOfValue' (I.VRecClosure (f,n,e,_)) d = 
        String.concat ["<function ", f, " (", n, ",", I.stringOfExpr e,")>"]
    | stringOfValue' (I.VNil) d = "[]"
    | stringOfValue' (I.VCons (v1,v2)) d = let
        fun str v1 v2 d = str' v1 (E.force v2) d 
  and str' _ _ 0 = "..."
          | str' v (I.VCons (v1,v2)) d = String.concat [stringOfValue v d,
                                               ",",
                                                     str v1 v2 (d-1)]
          | str' v _ d = String.concat [stringOfValue v d, "]"]
      in
        "["^(str v1 v2 d)
      end
    | stringOfValue' (I.VDelayed _) d = "<delayed>"


  fun run fenv = let
    fun prompt () = (print "project> "; TextIO.inputLine (TextIO.stdIn))
    fun pr l = print ((String.concatWith " " l)^"\n")
    fun read fenv = 
  (case prompt () 
    of NONE => ()
     | SOME ".\n" => ()
     | SOME str => eval_print fenv str)
    and eval_print fenv str = let
      fun process env str = let 
  val ts = P.lexString str
      in 
  case (P.parseDecl ts)
   of P.DDef (s,e) => let
        val v = E.eval env e
        val _ = pr ["Definition",s,"added to environment"]
      in
        ((s,v)::env)
      end
    | P.DExpr e => let
        val v = E.eval env e
        val _ = pr [stringOfValue v 10]
      in
        env
      end
      end
    in
      (case String.isPrefix ":parse " str
  of true => let val ts = P.lexString (String.extract (str, 6, NONE))
                       val _ = pr (["Tokens ="] @ (map P.stringOfToken ts)) 
                 val expr = P.parseExpr ts
                 val _ = pr [I.stringOfExpr (expr)]
                   in
                     read fenv
                   end
         | false => (case String.isPrefix ":use "str
          of true => let val name = Substring.string (Substring.dropr Char.isSpace (Substring.extract (str,5,NONE)))
         val _ = pr ["Using", name]
             val ins = TextIO.openIn (name)
             fun loop ins env = 
           case TextIO.inputLine ins
            of SOME line => loop ins (process env line)
             | NONE => env
             val env = loop ins fenv 
         in
           TextIO.closeIn ins;
           read env
         end
           | false => read (process fenv str)))
      handle P.Parsing msg => (pr ["Parsing error:", msg]; read fenv)
     | E.Evaluation msg => (pr ["Evaluation error:", msg]; read fenv)
     | IO.Io _ => (pr ["I/O error"]; read fenv)
    end
    val initEnv = E.initialEnv @ fenv
  in
    print "Type . by itself to quit\n";
    print "Type :parse <expr> to see the parse of expression <expr>\n";
    print "Initial environment: "; 
    app (fn (s,_) => (print (s^" "))) initEnv;
    print "\n";
    read initEnv
  end
     
end
