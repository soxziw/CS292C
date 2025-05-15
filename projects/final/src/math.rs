use egg::{define_language, Id, Symbol};

// Define our mathematical expression language
define_language! {
    pub enum Math {
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "^2" = Square(Id),
        Const(Symbol),
        Var(Symbol),
    }
}