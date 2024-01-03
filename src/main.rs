#![allow(non_snake_case)]
use std::{env, process::{exit, Command}, fs, io:: Write};

mod tokenizer;
mod parser;
mod generation;

fn main() {
    let input = env::args().nth(1);
    let input = Some("tests/else.ignis");
    if input.is_none() {
        eprintln!("Error: Filename not provided: Usage \"IgnisScript <file>\"");
        exit(-1);
    }
    let file = input.unwrap();

    if fs::File::open(&file).is_err() {
        eprintln!("Error: File \"{}\" not found", file);
        exit(-1);
    }

    let content = fs::read_to_string(&file).unwrap();

    println!("{}", content);

    let mut tokens = tokenizer::Tokenizer::new(content);

    tokens.tokenize();

    let tokens = tokens.get_tokens();


    println!("{:?}", tokens);

    let mut parser = parser::Parser::new(tokens.clone());

    let tree = parser.parse_prog();

    if tree.is_none(){
        eprintln!("Invalid program");
        exit(-1);
    }
    let tree = tree.unwrap();
    println!("{:?}", tree);

    let assembly = generation::Generator::new(tree).gen_prog();


    println!("{}", assembly);

    let mut file = fs::File::create("out.asm").unwrap();

    file.write_all(assembly.as_bytes()).unwrap();

    file.flush().unwrap();

    let mut child = Command::new("nasm").arg("-felf64").arg("out.asm").spawn().unwrap();
    println!("nasm -felf64 out.asm");
    let _ = child.wait();
    let mut child= Command::new("ld").arg("-o").arg("out").arg("out.o").spawn().unwrap();
    
    println!("ld -o out out.o");
    let _ = child.wait();
}


