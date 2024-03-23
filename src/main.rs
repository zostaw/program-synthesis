use strum_macros::EnumIter;

const DEBUG: bool = false;
const MAX_SEARCH_DEPTH: usize = 6;

#[derive(Debug, Default, EnumIter)]
enum Expr {
    Input,
    #[default]
    Zero,
    Inc(Box<Expr>),            // +1
    Half(Box<Expr>),           // Multiplication expression
    Add(Box<Expr>, Box<Expr>), // Addition expression
    Mul(Box<Expr>, Box<Expr>), // Multiplication expression
}

// Implement Clone instead of Copy for Expr
impl Clone for Expr {
    fn clone(&self) -> Self {
        match self {
            Expr::Input => Expr::Input,
            Expr::Zero => Expr::Zero,
            Expr::Inc(n) => Expr::Inc(n.clone()),
            Expr::Half(n) => Expr::Half(n.clone()),
            Expr::Add(lhs, rhs) => Expr::Add(lhs.clone(), rhs.clone()), // Clone the boxed expressions
            Expr::Mul(lhs, rhs) => Expr::Mul(lhs.clone(), rhs.clone()), // Clone the boxed expressions
        }
    }
}

// Evaluation function for the AST
fn eval_ast(expr: &Expr, input: f64) -> f64 {
    match expr {
        Expr::Zero => 0.0,
        Expr::Inc(n) => eval_ast(n, input) + 1.0,
        Expr::Half(n) => eval_ast(n, input) * 0.5,
        Expr::Add(lhs, rhs) => eval_ast(lhs, input) + eval_ast(rhs, input),
        Expr::Mul(lhs, rhs) => eval_ast(lhs, input) * eval_ast(rhs, input),
        Expr::Input => input,
    }
}

fn grow(plist: Vec<Expr>) -> Vec<Expr> {
    let mut new_plist = plist.clone();

    let mut product = Vec::new();
    for item1 in plist.clone() {
        for item2 in plist.clone() {
            product.push((item1.clone(), item2.clone()));
        }
    }

    for (lhs, rhs) in product {
        new_plist.push(Expr::Mul(Box::new(lhs.clone()), Box::new(rhs.clone())));
        new_plist.push(Expr::Add(Box::new(lhs.clone()), Box::new(rhs.clone())));
        new_plist.push(Expr::Inc(Box::new(lhs.clone())));
        new_plist.push(Expr::Half(Box::new(lhs.clone())));
    }

    if DEBUG {
        println!("\n\n\ngrow returns: {:?}\n\n\n", new_plist);
    }
    return new_plist;
}

fn elim_equvalents(plist: Vec<Expr>, inputs: &Vec<f64>) -> Vec<Expr> {
    let mut new_plist: Vec<Expr> = Vec::new();

    let mut outputs_outcomes: Vec<Vec<f64>> = Vec::new();

    for p in plist.clone() {
        let res = inputs.iter().map(|inp| eval_ast(&p, *inp)).collect();
        if !outputs_outcomes.contains(&res) {
            outputs_outcomes.push(res);
            new_plist.push(p);
        }
    }
    if DEBUG {
        println!("\n\n\nelim_equvalents returns: {:?}\n\n\n", new_plist);
    }
    return new_plist;
}

fn synthesize(inputs: Vec<f64>, outputs: Vec<f64>) -> Expr {
    println!("Inputs -> Outputs: {:?} -> {:?}", &inputs, &outputs);
    let input = inputs[0].clone();
    let output = outputs[0].clone();

    // list of possible expressions
    let mut plist: Vec<Expr> = vec![Expr::Input, Expr::Zero];

    for _ in 0..MAX_SEARCH_DEPTH {
        plist = grow(plist);
        plist = elim_equvalents(plist, &inputs);
        for p in plist.iter() {
            let eval_res = eval_ast(&p, input);
            if eval_res == output {
                // println!("  Found promissing program...");
                let res = inputs
                    .iter()
                    .zip(outputs.clone().into_iter())
                    .find_map(|(inp, out)| match eval_ast(&p, *inp) == out {
                        true => {
                            // println!("    Matching {} -> {}", inp, out);
                            return None;
                        } // so good so far
                        false => {
                            // println!("    Failed on {} -> {}", inp, eval_ast(&p, inp));
                            return Some(1);
                        } // required single fail to disregard the program
                    });
                match res {
                    Some(_) => continue,
                    None => {
                        println!("      Program accespted:\n        AST: {:?}", p);
                        return p.to_owned();
                    }
                }
            }
        }
    }

    println!(
        "Could not synthesize function after {} steps.",
        MAX_SEARCH_DEPTH
    );
    return Expr::Zero;
}

fn main() {
    // First example - same
    println!("\nSynthesize Y=X function");
    let inputs: Vec<f64> = vec![1.0, 2.0, 8.0];
    let outputs: Vec<f64> = vec![1.0, 2.0, 8.0];
    let program = synthesize(inputs, outputs);
    println!(
        "      Test for program(10.0) = {}",
        eval_ast(&program, 10.0)
    );

    // Example -> zero
    println!("\nSynthesize Y=0 function");
    let inputs: Vec<f64> = vec![1.0, 2.0, 8.0];
    let outputs: Vec<f64> = vec![0.0, 0.0, 0.0];
    let program = synthesize(inputs, outputs);
    println!(
        "      Test for program(10.0) = {}",
        eval_ast(&program, 10.0)
    );

    // // Example -> increment
    // println!("\nSynthesize Y=X+1 function");
    // let inputs: Vec<f64> = vec![1.0, 2.0, 15.0];
    // let outputs: Vec<f64> = vec![2.0, 3.0, 16.0];
    // let program = synthesize(inputs, outputs);
    // println!(
    //     "      Test for program(10.0) = {}",
    //     eval_ast(&program, 10.0)
    // );

    // // Example -> double
    // println!("\nSynthesize Y=2*X function");
    // let inputs: Vec<f64> = vec![1.0, 2.0, 17.0];
    // let outputs: Vec<f64> = vec![2.0, 4.0, 34.0];
    // let program = synthesize(inputs, outputs);
    // println!(
    //     "      Test for program(10.0) = {}",
    //     eval_ast(&program, 10.0)
    // );

    // // Example -> x*7 + 1
    // println!("\nSynthesize Y=7*X+1 function");
    // let inputs: Vec<f64> = vec![1.0, 2.0, 0.5];
    // let outputs: Vec<f64> = vec![8.0, 15.0, 4.5];
    // let program = synthesize(inputs, outputs);
    // println!(
    //     "      Test for program(10.0) = {}",
    //     eval_ast(&program, 10.0)
    // );

    // // Example -> (x+1)*7
    // println!("\nSynthesize Y=7*(X+1) function");
    // let inputs: Vec<f64> = vec![1.0, 2.0, 5.0];
    // let outputs: Vec<f64> = vec![14.0, 21.0, 42.0];
    // let program = synthesize(inputs, outputs);
    // println!(
    //     "      Test for program(10.0) = {}",
    //     eval_ast(&program, 10.0)
    // );

    // Example -> (x)/2
    println!("\nSynthesize Y=0.5*X function");
    let inputs: Vec<f64> = vec![2.0, 4.0, 8.0];
    let outputs: Vec<f64> = vec![1.0, 2.0, 4.0];
    let program = synthesize(inputs, outputs);
    println!(
        "      Test for program(10.0) = {}",
        eval_ast(&program, 10.0)
    );

    // Example -> (x)/2 + 1
    println!("\nSynthesize Y=0.5*X+1 function");
    let inputs: Vec<f64> = vec![2.0, 4.0, 8.0];
    let outputs: Vec<f64> = vec![2.0, 3.0, 5.0];
    let program = synthesize(inputs, outputs);
    println!(
        "      Test for program(10.0) = {}",
        eval_ast(&program, 10.0)
    );

    // Example -> 2*((x)/2 + 1)
    println!("\nSynthesize Y=X**3 function");
    let inputs: Vec<f64> = vec![2.0, 4.0, 5.0];
    let outputs: Vec<f64> = vec![8.0, 64.0, 125.0];
    let program = synthesize(inputs, outputs);
    println!("      Test for program(3.0) = {}", eval_ast(&program, 3.0));
}
