use egg::{RecExpr, Id, Language};
use std::collections::HashMap;
use crate::math::Math;
use std::fs;

// Cost model for cryptographic operations
pub struct CryptoCost {
    // Cost weights for different operations
    add_cost: u64,
    sub_cost: u64,
    mul_cost: u64,
    square_cost: u64,
    costs: HashMap<String, u64>,
}

impl CryptoCost {
    pub fn new(costs: HashMap<String, u64>) -> Self {
        let add_cost = *costs.get("add").unwrap_or(&1);
        let sub_cost = *costs.get("sub").unwrap_or(&1);
        let mul_cost = *costs.get("mul").unwrap_or(&10);
        let square_cost = *costs.get("square").unwrap_or(&6);
        
        CryptoCost {
            add_cost,
            sub_cost,
            mul_cost,
            square_cost,
            costs,
        }
    }
    
    pub fn default() -> Self {
        let mut costs = HashMap::new();
        costs.insert("add".to_string(), 1);
        costs.insert("sub".to_string(), 1);
        costs.insert("mul".to_string(), 10);
        costs.insert("square".to_string(), 6);
        Self::new(costs)
    }

    pub fn cost_rec(&self, expr: &RecExpr<Math>) -> u64 {
        let mut costs = vec![0u64; expr.as_ref().len()];

        for (i, node) in expr.as_ref().iter().enumerate() {
            let children_cost = node
                .children()
                .iter()
                .map(|&id| costs[usize::from(id)])
                .sum::<u64>();

            let op_cost = match node {
                Math::Add(_) => self.add_cost,
                Math::Sub(_) => self.sub_cost,
                Math::Mul(_) => self.mul_cost,
                Math::Square(_) => self.square_cost,
                Math::Const(_) | Math::Var(_) => 0,
            };

            costs[i] = op_cost + children_cost;
        }

        *costs.last().unwrap()
    }

    // Implement Clone trait for CryptoCost
    pub fn clone(&self) -> Self {
        CryptoCost {
            add_cost: self.add_cost,
            sub_cost: self.sub_cost,
            mul_cost: self.mul_cost,
            square_cost: self.square_cost,
            costs: self.costs.clone(),
        }
    }
}

impl egg::CostFunction<Math> for CryptoCost {
    type Cost = u64;
    
    fn cost<C>(&mut self, enode: &Math, mut children_costs: C) -> u64
    where
        C: FnMut(Id) -> u64,
    {
        let children_cost: u64 = enode.fold(0, |sum, id| sum + children_costs(id));
        
        match enode {
            Math::Add(_) => self.add_cost + children_cost,
            Math::Sub(_) => self.sub_cost + children_cost,
            Math::Mul(_) => self.mul_cost + children_cost,
            Math::Square(_) => self.square_cost + children_cost,
            Math::Const(_) | Math::Var(_) => 0,
        }
    }
}

// Load cost model from a file
pub fn load_cost_model_from_file(filename: &str) -> CryptoCost {
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
    
    CryptoCost::new(costs)
}