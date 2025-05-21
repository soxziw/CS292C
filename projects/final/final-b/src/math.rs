use egg::{define_language, Id, Symbol, EGraph};

// Define our mathematical expression language
define_language! {
    pub enum Math {
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "^2" = Square(Id),
        Val(Symbol),
    }
}

pub fn is_const(egraph: &EGraph<Math, ()>, id: &Id) -> bool {
    // Check if the node is a constant (pure number) or a special variable (xi, gamma, beta)
    egraph[*id].nodes.iter().any(|node| {
        match node {
            Math::Val(sym) => {
                let name = sym.as_str();
                // Check if it's a number or one of the special variables
                name == "xi" || name == "gamma" || name == "beta" || name.parse::<i32>().is_ok()
            },
            _ => false
        }
    })
}