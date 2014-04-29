(* Map: LBRACKET, expr, BAR, SYM, LARROW, expr, RBRACKET *)
(* Filter: LBRACKET, expr, BAR, SYM, LARROW, expr, COMMA, expr, RBRACKET *) 
(* List: LBRACKET, expr_list, RBRACKET, RBRACKET *)
(* Interval: LBRACKET, expr, DDOTS, expr, RBRACKET*)


  and parse_aterm_COMBINED ts = 
    (case expect_LBRACKET ts
        of NONE => NONE
        | SOME ts =>
          ((updateSaved (!soFar) "<expr list>");(case parse_expr_list ts
            of NONE =>
              ((updateSaved (!soFar) "<expr>");(case parse_expr ts
                 of NONE => ((makeError "map" "expr" (!savedSoFar) [T_BAR] [] ts ) ; NONE)
                 | SOME (e1,ts) =>
                    (case expect_BAR ts
                       of NONE =>
                         (case expect_DDOTS ts
                           of NONE => 
                            (case expect_SYM ts
                              of NONE => ((makeError "map" "sym" [(!soFar)] [T_LARROW] [] ts ) ; NONE)
                              | SOME (s,ts) =>
                                (case expect_LARROW ts
                                  of NONE => ((makeError "map" "left arrow" [(!soFar)] [] ["expr"] ts ) ; NONE)
                                  | SOME ts =>
                                     ((updateSaved (!soFar) "<expr>");(case parse_expr ts 
                                        of NONE => ((makeError "map" "expr" (!savedSoFar) [T_RBRACKET] [] ts ) ; NONE)
                                        | SOME (e2,ts) =>
                                          (case expect_COMMA ts
                                            of NONE => 
                                              (case expect_RBRACKET ts
                                                 of NONE => ((makeError "map" "right bracket" [(!soFar)] [] [] [] ) ; NONE)
                                                 | SOME ts => SOME ((call2 "map" (I.EFun(s,e1)) e2),ts))
                                           | SOME ts => 
                                             ((updateSaved (!soFar) "<expr>");(case parse_expr ts
                                                of NONE => ((makeError "filter" "expr" (!savedSoFar) [T_RBRACKET] [] ts ); NONE)
                                                | SOME (e3,ts) =>
                                                  (case expect_RBRACKET ts 
                                                     of NONE => ((makeError "filter" "right bracket" [(!soFar)] [] [] [] ); NONE)
                                                     | SOME ts => let
                                                                      val x = (call2 "filter" (I.EFun(s,e3)) e2)
                                                                  in
                                                                      SOME (call2 "map" (I.EFun(s,e1)) x,ts)
                                                                  end))))))))))

                          | SOME ts =>
                            ((updateSaved (!soFar) "<expr>");(case parse_expr ts
                              of NONE => ((makeError "interval" "expr" (!savedSoFar) [T_RBRACKET] [] ts ); NONE)
                              | SOME (e2, ts) => 
                                (case expect_RBRACKET ts
                                  of NONE => ((makeError "interval" "right bracket" [(!soFar)] [] [] [] ); NONE)
                                   | SOME ts => SOME (call2 "interval" e1 e2, ts))))
           | SOME ts =>
             (case parse_expr_list ts
               of NONE =>  
               (case expect_RBRACKET ts
                of NONE => NONE 
                | SOME ts => SOME (I.EVal (I.VList []), ts))
                | SOME (es, ts) =>
                  (case expect_RBRACKET ts
                    of NONE => NONE
                     | SOME ts => SOME (es, ts)))))