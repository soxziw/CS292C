use egg::{Id, EGraph, Language, RecExpr};
use std::collections::HashMap;
use crate::math::Math;
use crate::math::{is_const_from_egraph, is_const_from_expr};
use std::fs;

// Cost model for cryptographic operations
pub struct CryptoCost<'a> {
    pub egraph: &'a EGraph<Math, ()>,
    pub add_cost: f64,
    pub sub_cost: f64,
    pub mul_cost: f64,
    pub square_cost: f64,
    pub const_mul_cost: f64,
    pub inv_cost: f64,
}

impl<'a> CryptoCost<'a> {
    pub fn new(egraph: &'a EGraph<Math, ()>, costs: HashMap<String, f64>) -> Self {
        let add_cost = *costs.get("add").unwrap_or(&1.0);
        let sub_cost = *costs.get("sub").unwrap_or(&1.0);
        let mul_cost = *costs.get("mul").unwrap_or(&10.0);
        let square_cost = *costs.get("square").unwrap_or(&6.0);
        let const_mul_cost = *costs.get("const_mul").unwrap_or(&4.0);
        let inv_cost = *costs.get("inv").unwrap_or(&80.0);

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
        costs.insert("add".to_string(), 1.0);
        costs.insert("sub".to_string(), 1.0);
        costs.insert("mul".to_string(), 10.0);
        costs.insert("square".to_string(), 6.0);
        costs.insert("const_mul".to_string(), 4.0);
        costs.insert("inv".to_string(), 80.0);
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

    pub fn cost_rec(&self, expr: &RecExpr<Math>) -> f64 {
        let mut costs = vec![0.0; expr.as_ref().len()];

        for (i, node) in expr.as_ref().iter().enumerate() {
            let children_cost = node
                .children()
                .iter()
                .map(|&id| costs[usize::from(id)] as f64)
                .sum::<f64>();

            let op_cost = match node {
                Math::Add([a, b]) => {
                    // Check if a and b are tuples
                    let is_tuple_a = matches!(&expr[*a], Math::Tuple2(_) | Math::Tuple3(_));
                    let is_tuple_b = matches!(&expr[*b], Math::Tuple2(_) | Math::Tuple3(_));
                    
                    if is_tuple_a || is_tuple_b {
                        // For tuple addition, count costs based on tuple structure
                        let mut tuple_add_cost = 2.0 * self.add_cost; // Default: two additions
                        
                        // Check if a is Tuple2(x, 0) or Tuple2(0, x)
                        let a_is_special = match &expr[*a] {
                            Math::Tuple2([x, y]) => {
                                let is_zero_x = matches!(&expr[*x], Math::Val(s) if s.as_str() == "0");
                                let is_zero_y = matches!(&expr[*y], Math::Val(s) if s.as_str() == "0");
                                is_zero_x || is_zero_y
                            },
                            _ => false
                        };
                        
                        // Check if b is Tuple2(x, 0) or Tuple2(0, x)
                        let b_is_special = match &expr[*b] {
                            Math::Tuple2([x, y]) => {
                                let is_zero_x = matches!(&expr[*x], Math::Val(s) if s.as_str() == "0");
                                let is_zero_y = matches!(&expr[*y], Math::Val(s) if s.as_str() == "0");
                                is_zero_x || is_zero_y
                            },
                            _ => false
                        };
                        
                        // If either a or b is special, only count one addition
                        if a_is_special || b_is_special {
                            tuple_add_cost = self.add_cost;
                        }
                        
                        tuple_add_cost
                    } else {
                        // Regular addition cost
                        self.add_cost
                    }
                },
                Math::Sub([a, b]) => {
                    // Check if a and b are tuples
                    let is_tuple_a = matches!(&expr[*a], Math::Tuple2(_) | Math::Tuple3(_));
                    let is_tuple_b = matches!(&expr[*b], Math::Tuple2(_) | Math::Tuple3(_));
                    
                    if is_tuple_a || is_tuple_b {
                        // For tuple addition, count costs based on tuple structure
                        let mut tuple_sub_cost = 2.0 * self.sub_cost; // Default: two additions
                        
                        // Check if a is Tuple2(x, 0) or Tuple2(0, x)
                        let a_is_special = match &expr[*a] {
                            Math::Tuple2([x, y]) => {
                                let is_zero_x = matches!(&expr[*x], Math::Val(s) if s.as_str() == "0");
                                let is_zero_y = matches!(&expr[*y], Math::Val(s) if s.as_str() == "0");
                                is_zero_x || is_zero_y
                            },
                            _ => false
                        };
                        
                        // Check if b is Tuple2(x, 0) or Tuple2(0, x)
                        let b_is_special = match &expr[*b] {
                            Math::Tuple2([x, y]) => {
                                let is_zero_x = matches!(&expr[*x], Math::Val(s) if s.as_str() == "0");
                                let is_zero_y = matches!(&expr[*y], Math::Val(s) if s.as_str() == "0");
                                is_zero_x || is_zero_y
                            },
                            _ => false
                        };
                        
                        // If either a or b is special, only count one addition
                        if a_is_special || b_is_special {
                            tuple_sub_cost = self.sub_cost;
                        }
                        
                        tuple_sub_cost
                    } else {
                        // Regular addition cost
                        self.sub_cost
                    }
                },
                Math::Mul([a, b]) => {
                    // Check if a and b are tuples
                    let is_tuple_a = matches!(&expr[*a], Math::Tuple2(_) | Math::Tuple3(_));
                    let is_tuple_b = matches!(&expr[*b], Math::Tuple2(_) | Math::Tuple3(_));
                    
                    if is_tuple_a || is_tuple_b {
                        // For tuple addition, count costs based on tuple structure
                        let mut tuple_mul_cost = 2.0 * self.mul_cost; // Default: two additions
                        
                        // Check if a is Tuple2(x, 0) or Tuple2(0, x)
                        let a_is_special = match &expr[*a] {
                            Math::Tuple2([x, y]) => {
                                let is_zero_x = matches!(&expr[*x], Math::Val(s) if s.as_str() == "0");
                                let is_zero_y = matches!(&expr[*y], Math::Val(s) if s.as_str() == "0");
                                is_zero_x || is_zero_y
                            },
                            _ => false
                        };
                        
                        // Check if b is Tuple2(x, 0) or Tuple2(0, x)
                        let b_is_special = match &expr[*b] {
                            Math::Tuple2([x, y]) => {
                                let is_zero_x = matches!(&expr[*x], Math::Val(s) if s.as_str() == "0");
                                let is_zero_y = matches!(&expr[*y], Math::Val(s) if s.as_str() == "0");
                                is_zero_x || is_zero_y
                            },
                            _ => false
                        };
                        
                        // If either a or b is special, only count one addition
                        if a_is_special || b_is_special {
                            tuple_mul_cost = self.mul_cost;
                        }
                        
                        tuple_mul_cost
                    } else {
                        let cost = if is_const_from_expr(expr, a) || is_const_from_expr(expr, b) {
                            self.const_mul_cost
                        } else {
                            self.mul_cost
                        };
                        
                        cost
                    }
                },
                Math::Square(_) => self.square_cost,
                Math::Val(_) => 0.0,
                Math::Inverse(_) => self.inv_cost,
                Math::Tuple2(_) => 0.0,
                Math::Tuple3(_) => 0.0,
            };

            costs[i] = op_cost + children_cost;
        }

        *costs.last().unwrap()
    }
}

impl<'a> egg::CostFunction<Math> for CryptoCost<'a> {
    type Cost = f64;
    
    fn cost<C>(&mut self, enode: &Math, mut children_costs: C) -> f64
    where
        C: FnMut(Id) -> f64,
    {
        let children_cost: f64 = enode.fold(0.0, |sum, id| sum + children_costs(id));
        
        match enode {
            Math::Add([a, b]) => {
                // Check if a and b are tuples
                let is_tuple_a = self.egraph[*a].nodes.iter().any(|node| matches!(node, Math::Tuple2(_) | Math::Tuple3(_)));
                let is_tuple_b = self.egraph[*b].nodes.iter().any(|node| matches!(node, Math::Tuple2(_) | Math::Tuple3(_)));
                
                if is_tuple_a || is_tuple_b {
                    2.0 * self.add_cost + children_cost
                } else {
                    // Regular addition cost
                    self.add_cost + children_cost
                }
            },
            Math::Sub([a, b]) => {
                // Check if a and b are tuples
                let is_tuple_a = self.egraph[*a].nodes.iter().any(|node| matches!(node, Math::Tuple2(_) | Math::Tuple3(_)));
                let is_tuple_b = self.egraph[*b].nodes.iter().any(|node| matches!(node, Math::Tuple2(_) | Math::Tuple3(_)));
                
                if is_tuple_a || is_tuple_b {
                    2.0 * self.sub_cost + children_cost
                } else {
                    // Regular addition cost
                    self.sub_cost + children_cost
                }
            },
            Math::Mul([a, b]) => {
                // Check if a and b are tuples
                let is_tuple_a = self.egraph[*a].nodes.iter().any(|node| matches!(node, Math::Tuple2(_) | Math::Tuple3(_)));
                let is_tuple_b = self.egraph[*b].nodes.iter().any(|node| matches!(node, Math::Tuple2(_) | Math::Tuple3(_)));
                
                if is_tuple_a || is_tuple_b {
                    // For tuple addition, count costs based on tuple structure
                    let mut tuple_mul_cost = 2.0 * self.mul_cost; // Default: two additions
                    
                    // Check if a is Tuple2(x, 0) or Tuple2(0, x)
                    let a_is_special = self.egraph[*a].nodes.iter().any(|node| {
                        if let Math::Tuple2([x, y]) = node {
                            let is_zero_x = self.egraph[*x].nodes.iter().any(|n| 
                                matches!(n, Math::Val(s) if s.as_str() == "0"));
                            let is_zero_y = self.egraph[*y].nodes.iter().any(|n| 
                                matches!(n, Math::Val(s) if s.as_str() == "0"));
                            is_zero_x || is_zero_y
                        } else {
                            false
                        }
                    });
                    
                    // Check if b is Tuple2(x, 0) or Tuple2(0, x)
                    let b_is_special = self.egraph[*b].nodes.iter().any(|node| {
                        if let Math::Tuple2([x, y]) = node {
                            let is_zero_x = self.egraph[*x].nodes.iter().any(|n| 
                                matches!(n, Math::Val(s) if s.as_str() == "0"));
                            let is_zero_y = self.egraph[*y].nodes.iter().any(|n| 
                                matches!(n, Math::Val(s) if s.as_str() == "0"));
                            is_zero_x || is_zero_y
                        } else {
                            false
                        }
                    });
                    
                    // If either a or b is special, only count one addition
                    if a_is_special || b_is_special {
                        tuple_mul_cost = self.mul_cost;
                    }
                    
                    tuple_mul_cost + children_cost
                } else {
                    let cost = if is_const_from_egraph(self.egraph, a) || is_const_from_egraph(self.egraph, b) {
                        self.const_mul_cost
                    } else {
                        self.mul_cost
                    };
    
                    cost + children_cost
                }
            },
            Math::Square(_) => self.square_cost + children_cost,
            Math::Val(_) => 0.0,
            Math::Inverse(_) => self.inv_cost + children_cost,
            Math::Tuple2(_) => children_cost,
            Math::Tuple3(_) => children_cost,
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
                        if let Ok(cost) = parts[1].trim().parse::<f64>() {
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