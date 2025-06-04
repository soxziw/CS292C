use egg::{EGraph, Id, Pattern, RecExpr, Rewrite};
use z3::{Config, Context, Solver, ast::{Ast, Int}};
use crate::math::Math;
use crate::rules::{default_rules, load_rules_from_file};

pub fn verify_rule(rule: &Rewrite<Math, ()>) -> bool {
    let config = Config::new();
    let context = Context::new(&config);
    let solver = Solver::new(&context);

    let (lhs, rhs) = match extract_lhs_rhs(rule) {
        Some(pair) => pair,
        None => {
            println!("Could not extract lhs/rhs from rule '{}'", rule.name);
            return false;
        }
    };

    let lhs_expr = expr_to_z3(lhs, &context);
    let rhs_expr = expr_to_z3(rhs, &context);

    solver.assert(&lhs_expr._eq(&rhs_expr).not());

    match solver.check() {
        z3::SatResult::Sat => {
            println!("Rule '{}' is NOT valid. Counterexample: {:?}", rule.name, solver.get_model());
            false
        },
        z3::SatResult::Unsat => {
            println!("Rule '{}' is valid.", rule.name);
            true
        },
        z3::SatResult::Unknown => {
            println!("Rule '{}' verification is inconclusive.", rule.name);
            false
        }
    }
}

fn extract_lhs_rhs(rule: &Rewrite<Math, ()>) -> Option<(&RecExpr<Math>, &RecExpr<Math>)> {
    match rule.name.as_str() {
        // Handle rules in the format "(^2 ?a) => (* ?a ?a)"
        name if name.contains("=>") => {
            let parts: Vec<&str> = name.split("=>").collect();
            if parts.len() == 2 {
                let lhs_str = parts[0].trim();
                let rhs_str = parts[1].trim();
                
                // Parse the expressions using RecExpr::from_str
                let lhs = RecExpr::<Math>::from_str(lhs_str).ok()?;
                let rhs = RecExpr::<Math>::from_str(rhs_str).ok()?;
                
                return Some((&lhs, &rhs));
            }
        }
        _ => {}
    }
}
fn expr_to_z3<'a>(expr: &RecExpr<Math>, ctx: &'a Context) -> Int<'a> {
    let mut cache: Vec<Option<Int<'a>>> = vec![None; expr.as_ref().len()];
    let mut var_map = std::collections::HashMap::new();

    for (i, node) in expr.as_ref().iter().enumerate() {
        let result = match node {
            Math::Add([a, b]) => {
                let a = cache[usize::from(*a)].as_ref().unwrap();
                let b = cache[usize::from(*b)].as_ref().unwrap();
                a + b
            },
            Math::Sub([a, b]) => {
                let a = cache[usize::from(*a)].as_ref().unwrap();
                let b = cache[usize::from(*b)].as_ref().unwrap();
                a - b
            },
            Math::Mul([a, b]) => {
                let a = cache[usize::from(*a)].as_ref().unwrap();
                let b = cache[usize::from(*b)].as_ref().unwrap();
                a * b
            },
            Math::Square(a) => {
                let a = cache[usize::from(*a)].as_ref().unwrap();
                a * a
            },
            Math::Inverse(a) => {
                let a = cache[usize::from(*a)].as_ref().unwrap();
                Int::new_const(ctx, format!("inv_{}", i)) * a
            },
            Math::Val(sym) => {
                let s = sym.as_str();
                if let Ok(n) = s.parse::<i64>() {
                    Int::from_i64(ctx, n)
                } else {
                    var_map
                        .entry(s.to_string())
                        .or_insert_with(|| Int::new_const(ctx, s))
                        .clone()
                }
            }
        };
        cache[i] = Some(result);
    }

    cache[expr.as_ref().len() - 1].as_ref().unwrap().clone()
}


pub fn verify_ruleset(rules: &[Rewrite<Math, ()>]) -> bool {
    let mut all_valid = true;
    for rule in rules {
        if !verify_rule(rule) {
            all_valid = false;
            println!("Rule verification failed: {}", rule.name);
        }
    }
    all_valid
}
