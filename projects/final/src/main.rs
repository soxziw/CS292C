mod costs;
mod rules;
mod math;

use egg::{Extractor, RecExpr, Runner};
use std::io::{self, BufRead};
use std::str::FromStr;
use crate::rules::{default_rules, load_rules_from_file};
use crate::costs::{CryptoCost, load_cost_model_from_file};
use crate::math::Math;

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
    
    let cost_model = match cost_file {
        Some(filename) => {
            println!("Loading cost model from file: {}", filename);
            load_cost_model_from_file(filename)
        },
        None => {
            println!("Using default cost model...");
            CryptoCost::default()
        },
    };

    loop{
        println!("Finite Field Expression Optimizer");
        println!("=================================");
        println!("Enter an expression to optimize (in S-expression format):");

        let stdin = io::stdin();
        let mut input = String::new();
        stdin.lock().read_line(&mut input)?;
        let expr_str = input.trim();

        println!("Optimizing expression: {}", expr_str);

        // Parse expression as RecExpr (not Pattern)
        let expr = RecExpr::<Math>::from_str(expr_str).expect("Failed to parse expression");

        // Calculate original cost
        let original_cost = cost_model.cost_rec(&expr);

        // Run egg optimizer
        let mut runner = Runner::default().with_expr(&expr);
        runner = runner.run(&rules);

        // Extract best-cost expression
        let extractor = Extractor::new(&runner.egraph, cost_model.clone());
        let (best_cost, best_expr) = extractor.find_best(runner.roots[0]);

        // Output
        println!("\nOptimization Results:");
        println!("---------------------");
        println!("Original expression: {}", expr_str);
        println!("Original cost: {}", original_cost);
        println!("Optimized expression: {}", best_expr);
        println!("Optimized cost: {}", best_cost);
        println!("Improvement: {:.2}%", (original_cost as f64 - best_cost as f64) / original_cost as f64 * 100.0);
        
        println!("\nE-Graph Statistics:");
        println!("Iterations: {}", runner.iterations.len());
        println!("Total e-graph nodes: {}", runner.egraph.total_size());
        println!("E-classes: {}", runner.egraph.number_of_classes());
    }
}

