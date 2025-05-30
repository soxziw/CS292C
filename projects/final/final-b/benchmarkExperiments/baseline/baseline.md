```
Finite Field Expression Optimizer
=================================
Enter an expression to optimize (in S-expression format):
(+ (* c b) (* c a))
formatted:
(+
  (* c b)
  (* c a))
(+ (+ (* a c) (* b c)) d)
formatted:
(+
  (+
    (* a c)
    (* b c))
  d)
done
Using default rules...
Using default cost model...

Optimization Results:
---------------------
Original expression 0:
(+
  (* c b)
  (* c a))
Optimized expression 0:
(*
  c
  (+ b a))
Original expression 1:
(+
  (+
    (* a c)
    (* b c))
  d)
Optimized expression 1:
(+
  d
  (*
    c
    (+ b a)))
Original total cost: 22
Optimized cost: 12
Improvement: 45.45%

E-Graph Statistics:
Iterations: 13
Total e-graph nodes: 10086
E-classes: 1982
```