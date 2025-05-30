COMMAND:
`echo -e "(- (* a0 b0) (* a1 b1))\n(- (- (* (+ a0 a1) (+ b0 b1)) (* a0 b0)) (* a1 b1))\ndone" | cargo run`

Full log:
```
Finite Field Expression Optimizer
=================================
Enter an expression to optimize (in S-expression format):
(- (* a0 b0) (* a1 b1))
formatted:
(-
  (* a0 b0)
  (* a1 b1))
(- (- (* (+ a0 a1) (+ b0 b1)) (* a0 b0)) (* a1 b1))
formatted:
(-
  (-
    (*
      (+ a0 a1)
      (+ b0 b1))
    (* a0 b0))
  (* a1 b1))
done
Using default rules...
Using default cost model...

Optimization Results:
---------------------
Original expression 0:
(-
  (* a0 b0)
  (* a1 b1))
Optimized expression 0:
(-
  (* a0 b0)
  (* a1 b1))
Original expression 1:
(-
  (-
    (*
      (+ a0 a1)
      (+ b0 b1))
    (* a0 b0))
  (* a1 b1))
Optimized expression 1:
(+
  (*
    b0
    (+
      a0
      (+
        a1
        (* a0 -1))))
  (*
    b1
    (+
      a1
      (+
        a0
        (* a1 -1)))))
Original total cost: 35
Optimized cost: 66
Improvement: -88.57%

E-Graph Statistics:
Iterations: 15
Total e-graph nodes: 10307
E-classes: 2372
```

Benchmark
```
v0 = a0*b0
v1 = a1*b1
v2 = (a0+a1)*(b0+b1)
return (v0 - v1, v2 - v0 - v1)
```
Important Costs in cost model
```
General Mul: 10
Addition/Subtraction: 1
Mul by constant: 4
```


Looking at the example, input was
`(a0+a1)*(b0+b1) - a0*b0 - a1*b1`

4 addition/subtraction
3 general multiplication

**Total Cost:** 4 + 30 = 34

We're getting
`(b0 * (a0 + a1 + a0*-1)) +
  (b1 * (a1 + a0 + a1*-1))`
Semantically it is equivalent to the original expression ie
`(b0 * a1) + (b1 * a0)`
\=
`(a0+a1)*(b0+b1) - a0*b0 - a1*b1`

`a0*b0 + a0*b1 + a1*b0 + a1*b1 - a0*b0 - a1*b1`
`a0*b1 + a1*b0`

2 general muls
5 addition/subtraction
2 mul by constant
**Total Cost:** 20 + 5 + 8 = 33

ADDED
`rewrite!("add-cancel"; "(+ ?a (* -1 ?a))" => "0"),`
to remove the a1 + a1*-1

New output removes the a1 + a1*-1 nicely:
```
Finite Field Expression Optimizer
=================================
Enter an expression to optimize (in S-expression format):
(- (* a0 b0) (* a1 b1))
formatted:
(-
  (* a0 b0)
  (* a1 b1))
(- (- (* (+ a0 a1) (+ b0 b1)) (* a0 b0)) (* a1 b1))
formatted:
(-
  (-
    (*
      (+ a0 a1)
      (+ b0 b1))
    (* a0 b0))
  (* a1 b1))
done
Using default rules...
Using default cost model...

Optimization Results:
---------------------
Original expression 0:
(-
  (* a0 b0)
  (* a1 b1))
Optimized expression 0:
(-
  (* a0 b0)
  (* a1 b1))
Original expression 1:
(-
  (-
    (*
      (+ a0 a1)
      (+ b0 b1))
    (* a0 b0))
  (* a1 b1))
Optimized expression 1:
(+
  (* a0 b1)
  (* b0 a1))
Original total cost: 35
Optimized cost: 42
Improvement: -20.00%

E-Graph Statistics:
Iterations: 18
Total e-graph nodes: 19708
E-classes: 6145
```

**Improvement still -20% since caching the original result may lead to BETTER performance than actually optimizing it!**


Without caching:
```
Finite Field Expression Optimizer
=================================
Enter an expression to optimize (in S-expression format):
formatted:
(-
  (* a0 b0)
  (* a1 b1))
formatted:
(-
  (-
    (*
      (+ a0 a1)
      (+ b0 b1))
    (* a0 b0))
  (* a1 b1))
Using default rules...
Using default cost model...

Optimization Results:
---------------------
Original expression 0:
(-
  (* a0 b0)
  (* a1 b1))
Optimized expression 0:
(-
  (* a0 b0)
  (* a1 b1))
Original expression 1:
(-
  (-
    (*
      (+ a0 a1)
      (+ b0 b1))
    (* a0 b0))
  (* a1 b1))
Optimized expression 1:
(+
  (* a0 b1)
  (* b0 a1))
Original total cost: 49
Optimized cost: 42
Improvement: 14.29%

E-Graph Statistics:
Iterations: 18
Total e-graph nodes: 19708
E-classes: 6145
```