let String = { mempty = ""; mappend = a: b: a + b; };
    List = { singleton = x: [x]; };
    pair = a: b: { _1 = a; _2 = b; };
    Pair = { pure = s: pair String.mempty s;
             mappend = a: b: pair (String.mappend a._1 b._1) b._2;
             map = f: p: pair (f p._1) p._2;
           };
    compose = f: g: x: f (g x);
in with builtins; rec {
  # Parser a = String -> [(a, String)]
  string = str: s:
    let strL = stringLength str;
        sL = stringLength s;
    in if str == substring 0 strL s
       then [ (pair str (substring strL sL s)) ]
       else [];

  regex = capture_groups: re: s:
    let result = match "(${re})(.*)" s;
    in if result == null
       then []
       else [ (pair (elemAt result 0) (elemAt result (1 + capture_groups))) ];

  # Left-biased choice
  choice = cs: s:
    if cs == []
    then []
    else let c = head cs;
             cs' = tail cs;
             result = c s;
         in if result == []
            then choice cs' s
            else result;

  optional = p: s:
    let result = p s;
    in if result == []
      then [ (Pair.pure s) ]
      else result;

  chain = ps: s:
    if ps == []
    then [ (Pair.pure s) ]
    else let p = head ps;
             ps' = tail ps;
             result = p s;
         in if result == []
            then []
            else let r = head result;
                     rest = chain ps' r._2;
                 in if rest == []
                    then []
                    else let r' = head rest;
                         in [ (Pair.mappend r r') ];

  idstring = regex 0 "[[:alnum:]-\.]+";

  license-ref = let documentref = chain [ (string "DocumentRef-")
                                          idstring
                                          (string ":")
                                        ];
    in chain [
      (optional documentref)
      (string "LicenseRef-")
      idstring
    ];

  simple-expression = choice [ license-ref
                               (chain [ idstring (string "+") ])
                               idstring
                             ];

  compound-expression = let wrap = compose (map (Pair.map List.singleton));
    in choice [
      (s: let result = simple-expression s;
          in if result == []
             then []
             else let r = head result;
                      rest = chain [(string " WITH ") idstring ] r._2;
                  in if rest == []
                     then []
                     else [(pair r._1 (head rest)._2)]
      )
      (s: let result = simple-expression s;
          in if result == []
             then []
             else let r = head result;
                      firstLicense = r._1;
                      operator = choice [ (string " AND ")
                                          (string " OR ")
                                        ]
                                        r._2;
                  in if operator == []
                     then []
                     else let s' = (head operator)._2;
                              licenses = compound-expression s';
                          in if licenses == []
                             then []
                             else let ls = head licenses;
                                  in [(pair ([firstLicense] ++ ls._1) ls._2)]
      )
      (wrap simple-expression)
      (s: let openParen = string "(" s;
          in if openParen == []
             then []
             else let result = compound-expression (head openParen)._2;
                  in if result == []
                     then []
                     else let r = head result;
                              closeParen = string ")" r._2;
                          in if closeParen == []
                             then []
                             else [(pair r._1 (head closeParen)._2)])
    ];
}
