mod costs;
mod rules;
mod math;
mod verify;
use egg::{Extractor, RecExpr, Runner};
use std::io::{self, BufRead};
use std::str::FromStr;
use crate::rules::{default_rules, load_rules_from_file};
use crate::costs::{CryptoCost, load_cost_model_from_file};
use crate::math::Math;
use crate::verify::verify_ruleset;
fn main() -> io::Result<()> {
    // Parse command line arguments
    let args: Vec<String> = std::env::args().collect();
    
    // Default filenames
    let mut rules_file = None;
    let mut cost_file = None;
    
    // Check for command line arguments
    if args.len() > 1 {
        rules_file = Some(&args[1]);
    }
    
    if args.len() > 2 {
        cost_file = Some(&args[2]);
    }

    println!("Finite Field Expression Optimizer");
    println!("=================================");
    println!("Enter an expression to optimize (in S-expression format):");

    let mut runner = Runner::default();
    let stdin = io::stdin();
    let mut input = String::new();
    stdin.lock().read_line(&mut input)?;
    let expr_str = input.trim();

    
    let expr: RecExpr<Math> = RecExpr::<Math>::from_str(expr_str).expect("Failed to parse expression");

    println!("formatted:");
    println!("{}", expr.pretty(10));

    // Run egg optimizer
    runner = runner.with_expr(&expr);

    // If files are provided, use them to load custom rules and costs
    // Otherwise, use defaults
    let rules = match rules_file {
        Some(filename) => {
            println!("Loading rules from file: {}", filename);
            load_rules_from_file(filename)
        },
        None => {
            println!("Using default rules...");
            default_rules()
        },
    };

    runner = runner.run(&rules);
    
    let cost_model = match cost_file {
        Some(filename) => {
            println!("Loading cost model from file: {}", filename);
            load_cost_model_from_file(&runner.egraph, filename)
        },
        None => {
            println!("Using default cost model...");
            CryptoCost::default(&runner.egraph)
        },
    };

    // Calculate costs for all expressions
    let total_original_cost = cost_model.clone().cost_rec(&expr);

    // Extract the best expressions
    let extractor = Extractor::new(&runner.egraph, cost_model.clone());
    let (total_cost, best_expr) = extractor.find_best(runner.roots[0]);

    if total_cost >= total_original_cost {
        println!("Optimized expression:\n{}", best_expr.pretty(10));
        println!("Optimized cost: {}", total_cost);
        println!("No improvement found");
        return Ok(());
    }

    // Output
    println!("\nOptimization Results:");
    println!("---------------------");
    println!("Original expression:\n{}", expr.pretty(10));
    println!("Optimized expression:\n{}", best_expr.pretty(10));
    println!("Original total cost: {}", total_original_cost);
    println!("Optimized cost: {}", total_cost);
    println!("Improvement: {:.2}%", (total_original_cost as f64 - total_cost as f64) / total_original_cost as f64 * 100.0);
    
    println!("\nE-Graph Statistics:");
    println!("Iterations: {}", runner.iterations.len());
    println!("Total e-graph nodes: {}", runner.egraph.total_size());
    println!("E-classes: {}", runner.egraph.number_of_classes());

    Ok(())
}

