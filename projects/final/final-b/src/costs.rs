use egg::{Id, EGraph, Language, RecExpr};
use std::collections::HashMap;
use crate::math::Math;
use crate::math::{is_const_from_expr, is_const_from_egraph};
use std::fs;

// Cost model for cryptographic operations
pub struct CryptoCost<'a> {
    pub egraph: &'a EGraph<Math, ()>,
    pub add_cost: u64,
    pub sub_cost: u64,
    pub mul_cost: u64,
    pub square_cost: u64,
    pub const_mul_cost: u64,
    pub inv_cost: u64,
}

impl<'a> CryptoCost<'a> {
    pub fn new(egraph: &'a EGraph<Math, ()>, costs: HashMap<String, u64>) -> Self {
        let add_cost = *costs.get("add").unwrap_or(&1);
        let sub_cost = *costs.get("sub").unwrap_or(&1);
        let mul_cost = *costs.get("mul").unwrap_or(&10);
        let square_cost = *costs.get("square").unwrap_or(&6);
        let const_mul_cost = *costs.get("const_mul").unwrap_or(&4);
        let inv_cost = *costs.get("inv").unwrap_or(&80);

        CryptoCost {
            egraph,
            add_cost,
            sub_cost,
            mul_cost,
            square_cost,
            const_mul_cost,
            inv_cost,
        }
    }
    
    pub fn default(egraph: &'a EGraph<Math, ()>) -> Self {
        let mut costs = HashMap::new();
        costs.insert("add".to_string(), 1);
        costs.insert("sub".to_string(), 1);
        costs.insert("mul".to_string(), 10);
        costs.insert("square".to_string(), 6);
        costs.insert("const_mul".to_string(), 4);
        costs.insert("inv".to_string(), 80);
        Self::new(egraph, costs)
    }

    // Implement Clone trait for CryptoCost
    pub fn clone(&self) -> Self {
        CryptoCost {
            egraph: self.egraph,
            add_cost: self.add_cost,
            sub_cost: self.sub_cost,
            mul_cost: self.mul_cost,
            square_cost: self.square_cost,
            const_mul_cost: self.const_mul_cost,
            inv_cost: self.inv_cost,
        }
    }

    pub fn cost_of_node(&self, node: &Math, expr: &RecExpr<Math>) -> u64 {
        match node {
            Math::Add(_) => self.add_cost,
            Math::Sub(_) => self.sub_cost,
            Math::Mul([a, b]) => {
                if is_const_from_expr(expr, a) || is_const_from_expr(expr, b) {
                    self.const_mul_cost
                } else {
                    self.mul_cost
                }
            },
            Math::Square(_) => self.square_cost,
            Math::Val(_) => 0,
            Math::Inverse(_) => self.inv_cost,
        }
    }
}

impl<'a> egg::CostFunction<Math> for CryptoCost<'a> {
    type Cost = u64;
    
     fn cost<C>(&mut self, enode: &Math, mut children_costs: C) -> u64
    where
        C: FnMut(Id) -> u64,
    {
        let children_cost: u64 = enode.fold(0, |sum, id| sum + children_costs(id));
        
        match enode {
            Math::Add(_) => self.add_cost + children_cost,
            Math::Sub(_) => self.sub_cost + children_cost,
            Math::Mul([a, b]) => {
                let cost = if is_const_from_egraph(self.egraph, a) || is_const_from_egraph(self.egraph, b) {
                    self.const_mul_cost
                } else {
                    self.mul_cost
                };

                cost + children_cost
            }
            Math::Square(_) => self.square_cost + children_cost,
            Math::Val(_) => 0,
            Math::Inverse(_) => self.inv_cost + children_cost,
        }
    }
}

// Load cost model from a file
pub fn load_cost_model_from_file<'a>(egraph: &'a EGraph<Math, ()>, filename: &str) -> CryptoCost<'a> {
    let mut costs = HashMap::new();
    
    match fs::read_to_string(filename) {
        Ok(contents) => {
            for line in contents.lines() {
                let line = line.trim();
                if !line.is_empty() && !line.starts_with("#") {
                    let parts: Vec<&str> = line.split('=').collect();
                    if parts.len() == 2 {
                        let op = parts[0].trim().to_string();
                        if let Ok(cost) = parts[1].trim().parse::<u64>() {
                            costs.insert(op, cost);
                        }
                    }
                }
            }
        },
        Err(e) => {
            eprintln!("Error reading cost model file {}: {}", filename, e);
        }
    }
    
    CryptoCost::new(egraph, costs)
}