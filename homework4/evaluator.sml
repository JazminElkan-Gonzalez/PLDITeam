(* 
 *   CODE FOR HOMEWORK 4
 *)


structure Evaluator = struct

  structure I = InternalRepresentation



  exception Evaluation of string

  fun evalError msg = raise Evaluation msg


  (* 
   *   Primitive operations
   *)


  fun primCons a (I.VList xs) = I.VList (a::xs)
    | primCons a _ = evalError "primCons"

  fun primIntHelp (I.VList ks) (I.VInt j) (I.VInt newI) = if ((newI - 1) = j ) then (I.VList ks) else (primIntHelp (primCons (I.VInt j) (I.VList ks)) (I.VInt (j-1)) (I.VInt (newI)))
    | primIntHelp _ _ _= evalError "primIntHelp"

  fun primInterval (I.VInt i) (I.VInt j) = if (j < i ) then (I.VList []) else (primIntHelp (I.VList []) (I.VInt j) (I.VInt i))
    | primInterval _ _ = evalError "primInterval"

 fun primHd (I.VList []) = evalError "empty at PrimHd"
    | primHd (I.VList (x::xs)) = x
    | primHd _ = evalError "primHd"

fun primTl (I.VList []) = I.VList []
    | primTl (I.VList (x::xs)) = I.VList xs
    | primTl _ = evalError "primTl"

  fun primPlus (I.VInt a) (I.VInt b) = I.VInt (a+b)
    | primPlus _ _ = evalError "primPlus"

  fun primMinus (I.VInt a) (I.VInt b) = I.VInt (a-b)
    | primMinus _ _ = evalError "primMinus"



  fun primEqHelper (I.VList []) (I.VList []) = true
     | primEqHelper (I.VList (x::xs)) (I.VList []) = false
     | primEqHelper (I.VList []) (I.VList (x::xs)) = false
     | primEqHelper (I.VList (x::xs)) (I.VList (y::ys)) = 
    (case (primEq x y) 
        of (I.VBool false) => false 
        | (I.VBool true) => (primEqHelper (I.VList xs) (I.VList ys))
        | _ => evalError "how the fuck did you get here?????? primEqHelper")
     | primEqHelper _ _ = evalError "primEqHelper"

  and primEq (I.VInt a) (I.VInt b) = I.VBool (a=b)
    | primEq (I.VBool a) (I.VBool b) = I.VBool (a=b)
    | primEq (I.VList (xs)) (I.VList (ys)) = I.VBool (primEqHelper (I.VList xs) (I.VList ys)) 
    | primEq _ _ = I.VBool false

  fun primLess (I.VInt a) (I.VInt b) = I.VBool (a<b)
    | primLess _ _ = I.VBool false

			 
  fun lookup (name:string) [] = evalError ("failed lookup for "^name)
    | lookup name ((n,v)::env) = 
        if (n = name) then 
	  v
	else lookup name env 


  (*
   *   Evaluation functions
   * 
   *)


  fun eval _ (I.EVal v) = v
    | eval env (I.EFun (n,e)) = I.VClosure (n,e,env)
    | eval env (I.EIf (e,f,g)) = evalIf env (eval env e) f g
    | eval env (I.ELet (name,e,f)) = evalLet env name (eval env e) f
    | eval env (I.ELetFun (name,param,e,f)) = evalLetFun env name param e f
    | eval env (I.EIdent n) = lookup n env
    | eval env (I.EApp (e1,e2)) = evalApp env (eval env e1) (eval env e2)
    | eval env (I.EPrimCall1 (f,e1)) = f (eval env e1)
    | eval env (I.EPrimCall2 (f,e1,e2)) = f (eval env e1) (eval env e2)
    | eval env (I.ERecord fs) = evalError "ERecord not implemented"
    | eval env (I.EField (e,s)) = evalError "EField not implemented"
      
  and evalApp _ (I.VClosure (n,body,env)) v = eval ((n,v)::env) body
    | evalApp _ (I.VRecClosure (f,n,body,env)) v = let
	  val new_env = [(f,I.VRecClosure (f,n,body,env)),(n,v)]@env
      in 
	  eval new_env body
      end
    | evalApp _ _ _ = evalError "cannot apply non-functional value"

  and evalIf env (I.VBool true) f g = eval env f
    | evalIf env (I.VBool false) f g = eval env g
    | evalIf _ _ _ _ = evalError "evalIf"
		       
  and evalLet env id v body = eval ((id,v)::env) body

  and evalLetFun env id param expr body = let
      val f = I.VRecClosure (id, param, expr, env)
  in
      eval ((id,f)::env) body
  end


  (* 
   *   Initial environment (already in a form suitable for the environment)
   *)

  fun initMap (I.VClosure (n,e,env)) (I.VList []) = I.VList []
    | initMap (I.VClosure (n,e,env)) (I.VList (x::nil)) = I.VList [eval env (I.EApp (I.EFun (n,e),I.EVal x))]
    | initMap (I.VClosure (n,e,env)) (I.VList (x::xs)) = let
                                                           val vMap = (initMap (I.VClosure (n,e,env)) (I.VList (xs)))
                                                           fun mapL (I.VList xs) = xs
                                                             | mapL _ = evalError "initMap"
                                                         in
                                                            I.VList ((eval env (I.EApp (I.EFun (n,e),I.EVal x)))::(mapL vMap))
                                                         end
    | initMap _ _ = evalError "initMap"

  fun initFilter (I.VClosure (n,e,env)) (I.VList []) = I.VList ([])
    | initFilter (I.VClosure (n,e,env)) (I.VList (x::xs)) = let
                                                              val vFilter = (initFilter (I.VClosure (n,e,env)) (I.VList (xs)))
                                                              fun filterL (I.VList xs) = xs
                                                                | filterL _ = evalError "initFilter"
                                                              val xApp = (primEq (eval env (I.EApp (I.EFun (n,e),I.EVal x))) (I.VBool true))
                                                              fun checkEq (I.VBool x) = x
                                                                | checkEq _ = evalError "initFilter"
                                                            in
                                                              if (checkEq xApp)
                                                                then I.VList (x::(filterL vFilter))
                                                              else I.VList (filterL vFilter)
                                                            end
    | initFilter _ _ = evalError "initFilter"
  
  val initialEnv = 
      [("add", I.VClosure ("a", 
			   I.EFun ("b", 
				   I.EPrimCall2 (primPlus,
						 I.EIdent "a",
						 I.EIdent "b")),
			   [])),
       ("sub", I.VClosure ("a", 
			   I.EFun ("b", 
				   I.EPrimCall2 (primMinus,
						 I.EIdent "a",
						 I.EIdent "b")),
			   [])),
        ("interval", I.VClosure ("a", 
                           I.EFun ("b", 
                                   I.EPrimCall2 (primInterval,
                                                 I.EIdent "a",
                                                 I.EIdent "b")),
                           [])),
       ("hd", I.VClosure ("a", 
                                I.EPrimCall1 (primHd,
                                        I.EIdent "a"),
                           [])),
        ("tl", I.VClosure ("a", 
                                I.EPrimCall1 (primTl,
                                        I.EIdent "a"),
                           [])),
       ("cons", I.VClosure ("a", 
                           I.EFun ("b", 
                                   I.EPrimCall2 (primCons,
                                                 I.EIdent "a",
                                                 I.EIdent "b")),
                           [])),
       ("nil", I.VList []),
       ("map", I.VClosure ("f", I.EFun("xs",
                                  I.EPrimCall2 (initMap,
                                                I.EIdent "f",
                                                I.EIdent "xs")),
                          [])),
       ("filter", I.VClosure ("p", I.EFun("xs",
                                  I.EPrimCall2 (initFilter,
                                                I.EIdent "p",
                                                I.EIdent "xs")),
                          [])),
       ("equal", I.VClosure ("a",
			  I.EFun ("b",
				  I.EPrimCall2 (primEq,
						I.EIdent "a",
						I.EIdent "b")),
			  [])),
       ("less", I.VClosure ("a",
			    I.EFun ("b",
				    I.EPrimCall2 (primLess,
						  I.EIdent "a",
						  I.EIdent "b")),
			    []))]
  
				 
end
