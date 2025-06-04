use egg::{RecExpr, Id, Runner, Extractor, Language};
use std::collections::HashSet;
use std::str::FromStr;
use crate::math::Math;
use crate::rules::default_rules;
use crate::costs::CryptoCost;

fn subexpr_to_string(expr: &RecExpr<Math>, id: Id) -> String {
    let node = &expr[Id::from(id)];
    if node.children().is_empty() {
        node.to_string()
    } else {
        let children_strs: Vec<_> = node.children()
            .iter()
            .map(|&child_id| subexpr_to_string(expr, child_id))
            .collect();
        format!("({} {})", node.to_string(), children_strs.join(" "))
    }
}

fn subexpr_seen(subexpr: &String, seen: &HashSet<String>) -> bool {
    // Check if the subexpression is already in the seen set
    if seen.contains(subexpr) {
        return true;
    }
    
    let mut sorted_subexpr = subexpr.clone();
    let mut chars: Vec<_> = sorted_subexpr.chars().collect();
    chars.sort();
    sorted_subexpr = chars.into_iter().collect::<String>();

    // Use egg to verify if the subexpression is equivalent to any seen expression
    for seen_expr in seen.iter() {
        // Simple length check as a quick filter
        if seen_expr.len() != subexpr.len() {
            continue;
        }
        
        let mut sorted_seen_expr = seen_expr.clone();
        let mut chars: Vec<_> = sorted_seen_expr.chars().collect();
        chars.sort();
        sorted_seen_expr = chars.into_iter().collect::<String>();
        
        if sorted_seen_expr != sorted_subexpr {
            continue;
        }
        // Create expressions to check equivalence: (- subexpr seen_expr)
        if let (Ok(_), Ok(_)) = (RecExpr::<Math>::from_str(subexpr), RecExpr::<Math>::from_str(seen_expr)) {
            // If they're equivalent, their difference should be zero
            let diff_str = format!("(- {} {})", subexpr, seen_expr);
            if let Ok(diff_expr) = RecExpr::<Math>::from_str(&diff_str) {
                let mut runner = Runner::default();
                runner = runner.with_expr(&diff_expr);
                runner = runner.run(&default_rules());
                
                // If the expressions are equivalent, return true
                let extractor = Extractor::new(&runner.egraph, egg::AstSize);
                let (_, best) = extractor.find_best(runner.roots[0]);
                if best.to_string() == "0" {
                    return true;
                }
            }
        }
    }
    false
}

pub fn update_costs(expr: &RecExpr<Math>, seen: &mut HashSet<String>, total_cost: f64, cost_model: CryptoCost) -> f64 {
    let mut cost = total_cost;
    for (id, node) in expr.as_ref().iter().enumerate() {
        let subexpr = subexpr_to_string(&expr, Id::from(id));
        if !subexpr_seen(&subexpr, &seen) {
            seen.insert(subexpr.clone());
            let c = cost_model.clone().cost_of_node(node, expr);
            cost += c;
        }
    }
    cost
}