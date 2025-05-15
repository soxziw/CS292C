use egg::{Rewrite, Pattern};
use std::fs;
use std::str::FromStr;
use crate::math::Math;
use egg::rewrite;
// Default set of rewrite rules for finite field arithmetic
pub fn default_rules() -> Vec<Rewrite<Math, ()>> {
    vec![
        // Commutativity
        rewrite!("comm-add"; "(+ ?a ?b)" => "(+ ?b ?a)"),
        rewrite!("comm-mul"; "(* ?a ?b)" => "(* ?b ?a)"),
        
        // Associativity
        rewrite!("assoc-add"; "(+ ?a (+ ?b ?c))" => "(+ (+ ?a ?b) ?c)"),
        rewrite!("assoc-mul"; "(* ?a (* ?b ?c))" => "(* (* ?a ?b) ?c)"),
        
        // Distributivity
        rewrite!("distribute"; "(* ?a (+ ?b ?c))" => "(+ (* ?a ?b) (* ?a ?c))"),
        rewrite!("factor"; "(+ (* ?a ?b) (* ?a ?c))" => "(* ?a (+ ?b ?c))"),
        
        // Square identities
        rewrite!("square-def"; "(^2 ?a)" => "(* ?a ?a)"),
        rewrite!("square-mul"; "(* ?a ?a)" => "(^2 ?a)"),
        
        // 2ab identity
        rewrite!("2ab-forward"; "(* 2 (* ?a ?b))" => "(- (^2 (+ ?a ?b)) (+ (^2 ?a) (^2 ?b)))"),
        rewrite!("2ab-backward"; "(- (^2 (+ ?a ?b)) (+ (^2 ?a) (^2 ?b)))" => "(* 2 (* ?a ?b))"),
        
        // Karatsuba multiplication
        rewrite!("karatsuba"; "(+ (* ?a ?d) (* ?b ?c))" => 
                "(- (* (+ ?a ?b) (+ ?c ?d)) (+ (* ?a ?c) (* ?b ?d)))"),
    ]
}

// Parse a rule from string format
fn parse_rule(rule_str: &str) -> Option<Rewrite<Math, ()>> {
    let parts: Vec<&str> = rule_str.split("=>").collect();
    if parts.len() != 2 {
        eprintln!("Invalid rule format: {}", rule_str);
        return None;
    }
    
    let name = format!("rule_{}", rand::random::<u16>());
    let lhs = parts[0].trim();
    let rhs = parts[1].trim();
    
    match (Pattern::<Math>::from_str(lhs), Pattern::<Math>::from_str(rhs)) {
        (Ok(lhs_pattern), Ok(rhs_pattern)) => {
            Some(Rewrite::new(name, lhs_pattern, rhs_pattern).unwrap())
        },
        _ => {
            eprintln!("Failed to parse rule: {}", rule_str);
            None
        }
    }
}

// Load rules from a file
pub fn load_rules_from_file(filename: &str) -> Vec<Rewrite<Math, ()>> {
    let mut rules = Vec::new();
    
    match fs::read_to_string(filename) {
        Ok(contents) => {
            for line in contents.lines() {
                let line = line.trim();
                if !line.is_empty() && !line.starts_with("#") {
                    if let Some(rule) = parse_rule(line) {
                        rules.push(rule);
                    }
                }
            }
        },
        Err(e) => {
            eprintln!("Error reading rules file {}: {}", filename, e);
        }
    }
    
    rules
}