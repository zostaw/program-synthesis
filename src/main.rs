struct Synthesizer {
    training_inputs: [i32; 5],
    training_outputs: [i32; 5],
    grammar: Vec<fn(i32) -> i32>,
    program: Option<fn(i32) -> i32>,
}

impl Synthesizer {
    fn new(
        training_inputs: [i32; 5],
        training_outputs: [i32; 5],
        grammar: Vec<fn(i32) -> i32>,
    ) -> Self {
        Synthesizer {
            training_inputs,
            training_outputs,
            grammar,
            program: None,
        }
    }

    fn synthesize(&mut self) {
        for func in &self.grammar {
            let func_out = self
                .training_inputs
                .iter()
                .zip(self.training_outputs.iter())
                .rfold(0, |acc, (&inp, &out)| acc + func(inp) - out);
            dbg!("func_out: {}", &func_out);
            if func_out == 0 {
                self.program = Some(func.to_owned());
            }
        }
    }

    fn call(&self, input: i32) -> Option<i32> {
        match self.program {
            Some(program) => Some(program(input)),
            None => None,
        }
    }
}

fn double(a: i32) -> i32 {
    2 * a
}

fn main() {
    let mut functions: Vec<fn(i32) -> i32> = Vec::new();
    functions.push(double);

    let inputs: [i32; 5] = core::array::from_fn(|i| (i + 1) as i32);
    let outputs = inputs.clone().map(|i| i * 2);
    let mut program_synthesizer = Synthesizer::new(inputs, outputs, functions);

    program_synthesizer.synthesize();
    println!(
        "Synthesizing: {:?} -> {:?}\n    {:?}",
        program_synthesizer.training_inputs,
        program_synthesizer.training_outputs,
        program_synthesizer.program
    );

    // Test program
    let test_input = 17;
    println!(
        "Synthesizing: {:?} -> {:?}",
        test_input,
        program_synthesizer.call(test_input)
    );
}
