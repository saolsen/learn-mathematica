(* scheme *)
evalScheme[e_, v_ /; StringQ[v]] := v;
evalScheme[e_, v_ /; IntegerQ[v]] := v;
evalScheme[e_, True] := True;
evalScheme[e_, False] := False;
evalScheme[e_, v_ /; AtomQ[v]] := e[v];
evalScheme[e_, {define, s_, v_}] := e[s] = evalScheme[e, v];
evalScheme[e_, {lambda, args_, exp_}] := {args, exp};
evalScheme[e_, {fn_, inputs___}] := Module[{f, args, exp, rules},
  (* todo, eval arguements first *)
  {args, exp} = e[fn];
  rules = MapThread[Rule, {args, List[inputs]}];
  exp /. rules ]

evalScheme[env, {define, f, {lambda, {a, b}, a + b}}];
Print[evalScheme[env, {f, 1, 2}]]
