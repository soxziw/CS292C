# Eggcelerator: cracking cryptographic speed with e graphs

Given a cost model, we create a Domain Specific Language (DSL) and rewrite rules to find the cheapest logically equivalent form of an expression via algebraic rewrites 

---
### Prerequisites

1. Download [Rust](https://www.rust-lang.org/tools/install)
2. Build all dependencies
```bash
cd src
cargo build
```

### Tutorial
TL;DR Quick Command for B1:
```bash
echo -e "(+ (* a0 a0) (* (* a1 a1) x1))\n(* (* a0 2) a1)\ndone" | cargo run ../rules/rules_fp2.txt ../costs/costs_fp2.txt
```
1. Run the program 
```bash
cargo run LocationOfRules* LocationOfCostModel*
```
*These fields are optional and will be assigned to an fp2 default if not inputted

Output:
```
Finite Field Expression Optimizer
=================================
Enter an expression to optimize (in S-expression format):
formatted:
```
2. Write your S-expressions and when finished type "done"
```
(+ (* a0 a0) (* (* a1 a1) x1))
```

```
(* (* a0 2) a1)
```
```
done
```
3. You will now see the optimized expressions as well as their costs
```
Optimization Results:
---------------------
Original expression 0:
(+
  (* a0 a0)
  (*
    (* a1 a1)
    x1))
Optimized expression 0:
(+
  (^2 a0)
  (*
    (^2 a1)
    x1))
Original expression 1:
(*
  (* a0 2)
  a1)
Optimized expression 1:
(*
  a1
  (+ a0 a0))
Original total cost: 45
Optimized cost: 34
Improvement: 24.44%

E-Graph Statistics:
Iterations: 30
Total e-graph nodes: 9264
E-classes: 1719
```


### Branch-wise details
TENTATIVE
