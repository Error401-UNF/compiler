
// import from other files
// to build use cargo build
// Then the compiler generated is at target/debug/COMPILER
mod symbol_table;
mod lexical_analyzer;
mod parser;
mod type_checker;
mod syntax_evaluator;
mod Intermideate_generator;

use std::cell::RefCell;
// for collecting command line arguements
use std::env;
use std::fs;
use std::process::ExitCode;
use std::rc::Rc;

use parser::Parser;
use symbol_table::Env;
use lexical_analyzer::Lexer;
use type_checker::size_calculator;
use syntax_evaluator::Syntaxer;

use crate::syntax_evaluator::SyntaxTree;
use crate::syntax_evaluator::{IndexBox, SyntaxTreeNode};


fn main() -> ExitCode{
    let args:Vec<String> = env::args().collect();
    if !args.is_empty(){
        // something in the argument. expect 1 file arguement
        let file_to_parse = &args[1];
        let file = fs::read_to_string(file_to_parse);
        if file.is_ok(){
            let root = Env::new_root();
            let mut lexer = Lexer{
                root_env: Rc::clone(&root),
                all_scopes: vec![Rc::clone(&root)]
            };
            
            let lex_output= lexer.custom_lexer(&file.unwrap());
            if lex_output.is_err(){
                println!("lexing failed :(");
                println!("{}", lex_output.unwrap_err());
                return ExitCode::from(1);
            }else {
                Env::print_detailed_down(&lexer.root_env,0);
                //println!("{:?}", lex_output.clone().unwrap());
                println!("lexing finished");
                let parser = Parser {
                    all_scopes: lexer.all_scopes,
                };
                let res = parser.parse(lex_output.unwrap(), &lexer.root_env);
                if res.is_err(){
                    println!("Error: {}",res.unwrap_err());
                    return ExitCode::from(1);
                } else {
                    println!("Parsing Finished");
                    let mut controller = res.unwrap();
                    let size_out = size_calculator(&lexer.root_env);
                    println!("Size calculations\n{:?}",size_out.unwrap());
                
                    // start syntaxing
                    println!("Syntax Starting");
                    let controler_copy = controller.clone();
                    let root = controler_copy.get_weak(0);
                    let mut syntaxer = Syntaxer {
                        controler: controller,
                        valid_trees: Vec::new(),
                    };

                    // find all ast locations
                    syntaxer.find_all_trees(root.unwrap().clone());
                    let mut all_ast_trees: Vec<SyntaxTree> = Vec::new();
                    for tree_root in syntaxer.valid_trees.clone() {
                        let raw_tree  = syntaxer.make_tree(tree_root);
                        if raw_tree.is_ok() {
                            let mut ok_tree = raw_tree.unwrap();
                            let vector_clone = Rc::clone(&ok_tree.tree_vector);
                            let mut vector_borrow = vector_clone.borrow_mut();
                            let r  = syntaxer.fix_tree(&ok_tree, &mut vector_borrow,IndexBox::new(0));
                            if r.is_ok() {
                                ok_tree.top_node = r.unwrap();
                                all_ast_trees.push(ok_tree);
                            } else {
                                println!("tree fix error: {}", r.unwrap_err());
                                return ExitCode::from(1);
                            }
                        } else {
                            println!("tree parse error: {}",raw_tree.unwrap_err());
                            return ExitCode::from(1);
                        }
                    }
                    println!("tree displays");
                    for ast in all_ast_trees {
                        let ind = ast.top_node.get();
                        let tree_vec = ast.tree_vector.borrow();
                        let top_node = tree_vec[ind].borrow();
                        top_node.render_node(&tree_vec, 0,true);
                    }
                   
                    println!("Syntaxing finished");
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
