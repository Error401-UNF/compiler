
// import from other files
// to build use cargo build
// Then the compiler generated is at target/debug/COMPILER
mod symbol_table;
mod lexical_analyzer;
mod parser;



// for collecting command line arguements
use std::env;
use std::fs;
use std::process::ExitCode;

use crate::lexical_analyzer::Lexer;
use crate::parser::Parser;

fn main() -> ExitCode{
    let args:Vec<String> = env::args().collect();
    if !args.is_empty(){
        // something in the argument. expect 1 file arguement
        let file_to_parse = &args[1];
        let file = fs::read_to_string(file_to_parse);
        if file.is_ok(){
            let mut lexer = lexical_analyzer::Lexer{
                root_env: symbol_table::Env::new(None),
            };
            
            let lex_output= lexer.custom_lexer(&file.unwrap());
            if lex_output.is_err(){
                println!("lexing failed :(");
                println!("{}", lex_output.unwrap_err());
                return ExitCode::from(1);
            }else {
                lexer.root_env.detailed_print_all();

                println!("{:?}", lex_output.clone().unwrap());

                let res = Parser::parse(lex_output.unwrap(), lexer.root_env);
                if res.is_err(){
                    println!("Error: {}",res.unwrap_err());
                    return ExitCode::from(1);
                } else {
                    println!("Parsing Finished");
                    let controller = res.unwrap();
                    //controller.render_node(0); // node index 0 is always root of tree
                return ExitCode::from(0);
                }
            }

        }
        else {
            println!("Invalid file: {:?}", args)
        }
    }
    else {
        println!("no file given")
    }
    return ExitCode::from(1);
}

fn main2() {
    let code = "{int i;\n\twhile (true)\n\t\ti = i + 1;}";
    let mut par = Lexer{root_env:symbol_table::Env::new(None)};
    let result = par.custom_lexer(code).unwrap();
    println!("{}", Lexer::renderer(result.clone()));
    par.root_env.print_all();
    
    
    let res = Parser::parse(result, par.root_env);

    if res.is_err(){
        println!("Error: {}",res.unwrap_err())
    } else {
        println!("Parsing Finished");
        let controller = res.unwrap();
        controller.render_node(0); // node index 0 is always root of tree
    }

}

