
// import from other files
mod symbol_table;
mod lexical_analyzer;



// for collecting command line arguements
use std::env;
use std::fs;
use std::process::ExitCode;

fn main() -> ExitCode {
    let args:Vec<String> = env::args().collect();
    if !args.is_empty(){
        // something in the argument. expect 1 file arguement
        let file_to_parse = &args[1];
        let file = fs::read_to_string(file_to_parse);
        if file.is_ok(){
            let mut parser = lexical_analyzer::parser{
                root_env: symbol_table::Env::new(None),
            };
            
            let lex_output= parser.custom_lexer(&file.unwrap());
            if lex_output.is_err(){
                println!("Parsing failed :(");
                return ExitCode::from(1);
            }else {
                println!("{}", lexical_analyzer::parser::renderer(lex_output.unwrap()));
                println!("\nParsing completed successfully.\n\nSymbol Table:\n\n----");
                parser.root_env.print_all();
                return ExitCode::from(0);
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

