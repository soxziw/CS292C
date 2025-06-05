# Bench 1
echo -e "(+ (* a0 a0) (* (* a1 a1) x1))\n(* (* a0 2) a1)\ndone" | cargo run ../rules/rules_fp2.txt ../costs/costs_fp2.txt

# Bench 6
echo -e "(** (Fp6 g0 g1 g2 h0 h1 h2) (+ (- (** (Fp6 g0 g1 g2 h0 h1 h2) 4) (** (Fp6 g0 g1 g2 h0 h1 h2) 2)) 1))\ndone" | cargo run ../rules/rules_fp6.txt ../costs/costs_fp6.txt