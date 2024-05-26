#[derive(Debug)]
enum Expr {
    Num(i32),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
}

impl Clone for Expr {
    fn clone(&self) -> Self {
        match self {
            Expr::Num(n) => Expr::Num(n.clone()),
            Expr::Add(lhs, rhs) => Expr::Add(lhs.clone(), rhs.clone()),
            Expr::Mul(lhs, rhs) => Expr::Mul(lhs.clone(), rhs.clone()),
        }
    }
}

fn eval_ast(expression: &Expr) -> i32 {
    match expression {
        Expr::Num(n) => n.clone(),
        Expr::Mul(n1, n2) => eval_ast(n1) * eval_ast(n2),
        Expr::Add(n1, n2) => eval_ast(n1) + eval_ast(n2),
    }
}

fn main() {
    let equation: Expr = Expr::Add(
        Box::new(Expr::Mul(Box::new(Expr::Num(1)), Box::new(Expr::Num(3)))),
        Box::new(Expr::Mul(
            Box::new(Expr::Add(Box::new(Expr::Num(2)), Box::new(Expr::Num(3)))),
            Box::new(Expr::Num(5)),
        )),
    );

    let result = eval_ast(&equation);

    println!("Evaluating: {:?}", equation);
    println!("Output: {:?}", result);
}
