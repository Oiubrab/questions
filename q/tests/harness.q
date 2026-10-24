/ Minimal test harness shared by q/tests/test_*.q
/ Usage (from repo root): \l q/tests/harness.q, then .t.eq / .t.near / .t.ok, then .t.done[]
/ .t.done exits 1 if any assertion failed, so run_all.sh can detect failure.

\d .t

passes:0i
fails:0i

/ Record one result
report:{[name;okFlag;detail]
  $[okFlag;
    [.t.passes+:1i; -1 "  PASS ",name];
    [.t.fails+:1i;  -1 "  FAIL ",name,$[count detail; ": ",detail; ""]]]}

/ Exact match (value and type)
eq:{[name;got;want] report[name;got~want;"got ",(.Q.s1 got),", want ",.Q.s1 want]}

/ Float match within tolerance
near:{[name;got;want;tol] report[name;all tol>abs got-want;"got ",(.Q.s1 got),", want ",.Q.s1 want]}

/ Boolean condition
ok:{[name;cond] report[name;all cond;""]}

done:{[]
  -1 (string .t.passes)," passed, ",(string .t.fails)," failed";
  exit $[.t.fails>0i; 1; 0]}

\d .
