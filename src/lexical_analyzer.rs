// lexical_analyzer.rs
use std::rc::Rc;

use super::symbol_table::{Env,Value};

#[derive(Clone,PartialEq,Debug)]
pub enum tokens{
    // Blank
    Null,
    // data types
    Id(String),
    Basic(Value),
    Number(i32),
    Real(f32),

    // calling array
    ArrayEquationStart,
    ArrayEquationEnd,

    // static types
    True,
    False,

    // functions
    While,
    For,
    If,
    Else,
    Do,
    Break,

    // opperators
    Plus,
    Minus,
    Times,
    Assigns,
    Devides,

    Equals,
    Neq,
    Les,
    Leq,
    Gre,
    Geq,
    And,
    Or,

    Not,

    // space control
    Open, // pren
    Close,
    Stop,
    StartEnclose, // squigly
    EndEnclose,
}

// regx for all of the things


pub struct Lexer {
    pub(crate) root_env: Env
}

impl Lexer{
    pub fn custom_lexer(&mut self, raw_code:&str) -> Result<Vec<tokens>, String>{
        let mut out: Vec<tokens> = vec![];

        let mut code_left = raw_code;
        let mut local_env = Env::new(None);

        
        while code_left != "" {
            //println!("{}", code_left);
            for i in 0..code_left.len(){
                let segment =  &code_left[0..i+1];
                //println!("{}", segment);
                match segment {
                    " "|"\n"|"\t" => {code_left = &code_left[1..]; break;}, // ignore thoes symbols and remove that symbol

                    
                    // all of basic
                    "int" => {out.push(tokens::Basic(Value::AnyInt)); code_left = &code_left[segment.len()..]; break;}
                    "bool" => {out.push(tokens::Basic(Value::AnyBool)); code_left = &code_left[segment.len()..]; break;}
                    "float" => {out.push(tokens::Basic(Value::AnyFloat)); code_left = &code_left[segment.len()..]; break;}

                    // absalute values
                    "true" => {out.push(tokens::True); code_left = &code_left[segment.len()..]; break;}
                    "false" => {out.push(tokens::False); code_left = &code_left[segment.len()..]; break;}

                    // functions
                    "while" => {out.push(tokens::While); code_left = &code_left[segment.len()..]; break;}
                    "for" => {out.push(tokens::For); code_left = &code_left[segment.len()..]; break;}
                    "if" => {out.push(tokens::If); code_left = &code_left[segment.len()..]; break;}
                    "else" => {out.push(tokens::Else); code_left = &code_left[segment.len()..]; break;}
                    "do" => {out.push(tokens::Do); code_left = &code_left[segment.len()..]; break;}
                    "break" => {out.push(tokens::Break); code_left = &code_left[segment.len()..]; break;}
                    
                    // opperators
                    ";" => {out.push(tokens::Stop); code_left = &code_left[segment.len()..]; break;}
                    "+" => {out.push(tokens::Plus); code_left = &code_left[segment.len()..]; break;}
                    "-" => {out.push(tokens::Minus); code_left = &code_left[segment.len()..]; break;}
                    "= " => {out.push(tokens::Assigns); code_left = &code_left[segment.len()..]; break;}
                    "==" => {out.push(tokens::Equals); code_left = &code_left[segment.len()..]; break;}
                    "*" => {out.push(tokens::Times); code_left = &code_left[segment.len()..]; break;}
                    "/" => {out.push(tokens::Devides); code_left = &code_left[segment.len()..]; break;}
                    "(" => {out.push(tokens::Open); code_left = &code_left[segment.len()..]; break;}
                    ")" => {out.push(tokens::Close); code_left = &code_left[segment.len()..]; break;}
                    "{" => {out.push(tokens::StartEnclose); code_left = &code_left[segment.len()..]; break;}
                    "}" => {out.push(tokens::EndEnclose); code_left = &code_left[segment.len()..]; break;}
                    "< " => {out.push(tokens::Les); code_left = &code_left[segment.len()..]; break;}
                    "> " => {out.push(tokens::Gre); code_left = &code_left[segment.len()..]; break;}
                    "<=" => {out.push(tokens::Leq); code_left = &code_left[segment.len()..]; break;}
                    ">=" => {out.push(tokens::Geq); code_left = &code_left[segment.len()..]; break;}
                    "&&" => {out.push(tokens::And); code_left = &code_left[segment.len()..]; break;}
                    "||" => {out.push(tokens::Or); code_left = &code_left[segment.len()..]; break;}
                    "!=" => {out.push(tokens::Neq); code_left = &code_left[segment.len()..]; break;}
                    "! " => {out.push(tokens::Not); code_left = &code_left[segment.len()..]; break;}
                    "[" => {out.push(tokens::ArrayEquationStart); code_left = &code_left[segment.len()..]; break;}
                    "]" => {out.push(tokens::ArrayEquationEnd); code_left = &code_left[segment.len()..]; break;}

                    x if x.chars().last().eq(&Some('[')) && x.len() != 1 => {
                        // should be a known array variable
                        if local_env.get(x[..x.len()-1].to_owned()).is_some(){
                            // known variable
                            out.push(tokens::Id(x[..x.len()-1].to_owned()));
                            code_left = &code_left[segment.len()-1..];
                            break;
                        }
                        else {
                            println!("Array of non-known id");
                            code_left = &code_left[segment.len()-1..];
                            break;
                        }
                    }

                    // something that ends weird and not taken care of by something else
                    x if !is_last_char_letter(x) && x.len() > 1 => {

                        if local_env.get(x[..x.len()-1].to_owned()).is_some(){
                            // known variable
                            out.push(tokens::Id(x[..x.len()-1].to_owned()));
                            code_left = &code_left[segment.len()-1..];
                            break;
                        }


                        // we have a set of something then the full stop or space or end of file. This would be an expression we couldnt parse individually.

                        if let Some(&tokens::Basic(_)) = out.last() {
                            // default to id
                            let last_token = out.last().unwrap();
                            match last_token {
                                tokens::Basic(v) => {
                                    local_env.put(x[..x.len()-1].to_owned(), v.clone());
                                    out.push(tokens::Id(x[..x.len()-1].to_owned()));
                                    // full stop afterwards?
                                    code_left = &code_left[segment.len()..];
                                    if x.chars().last().eq(&Some(';')){ out.push(tokens::Stop);}
                                    break;
                                }
                                _ => {
                                    println!("Token error. got {}", segment);
                                    code_left = &code_left[segment.len()..];
                                    break;
                                }

                            }
                            
                        }

                        

                         // check for incomplete array
                        if let Some(tokens::ArrayEquationEnd) = out.last() {
                            // new eq and its likly a [i again
                            if Some('[') == x.chars().nth(0){
                                // throw in an eq starter and keep parsing
                                out.push(tokens::ArrayEquationStart);
                                code_left = &code_left[1..];
                                break;
                            } else if Some(';') == x.chars().last() || Some(' ') == x.chars().last(){
                                // its a new id.
                                let id_value = Self::compile_arr(&out);
                                if id_value.is_ok(){
                                    local_env.put(x[..x.len()-1].to_owned(), id_value.unwrap());
                                    out.push(tokens::Id(x[..x.len()-1].to_owned()));
                                    if x.chars().last().eq(&Some(';')){ out.push(tokens::Stop);}
                                    
                                } else {
                                    println!("Id Value Error: {}",segment)
                                }
                                code_left = &code_left[segment.len()..];
                                break;
                            } else {
                                println!("Array lexing error: {}",segment)
                            }
                        }

                        // number?
                        let num_string = &x[..x.len()-1];
                        let i = num_string.parse::<i32>();
                        let r = num_string.parse::<f32>();    
                        if i.is_ok(){
                            out.push(tokens::Number(i.unwrap()));
                            code_left = &code_left[segment.len()..];
                            if x.chars().last().eq(&Some(';')){ out.push(tokens::Stop);}
                            if x.chars().last().eq(&Some(')')){ out.push(tokens::Close);}
                            if x.chars().last().eq(&Some(']')){ out.push(tokens::ArrayEquationEnd);}
                            break;
                        } else if r.is_ok(){
                            out.push(tokens::Real(r.unwrap()));
                            code_left = &code_left[segment.len()..];
                            if x.chars().last().eq(&Some(';')){ out.push(tokens::Stop);}
                            if x.chars().last().eq(&Some(')')){ out.push(tokens::Close);}
                            if x.chars().last().eq(&Some(']')){ out.push(tokens::ArrayEquationEnd);}
                            break;
                        }
                        return Err(format!("problem with segment {:?}: {:?}",segment, out));
                    }




                    _ => {
                        if i == code_left.len()-1 {
                            //no more things to grab. Remove everything
                            println!("removed the following extra {}",segment);
                            code_left = "";
                            break;
                        }
                        continue; // assume i need more
                    }
                }
            }
        }
        // pakage env up for the future
        self.root_env = local_env;

        return Ok(out);
    }

    fn compile_arr(out:&Vec<tokens>) -> Result<Value, String>{
        // go backwards until you find a basic. any numbers / ids are taken note of
        let mut current_position = out.len() - 1;
        let mut sizes: Vec<i32> = vec![];
        let basic_val:Value;
        loop{

            if let tokens::Basic( v ) = &out[current_position] {
                basic_val = v.clone();
                break;
            } else {
                match  &out[current_position]  {
                    // you can have either something of size number or of an id. everytning else is thrown out
                    tokens::Number( n) => {
                        sizes.push(*n)
                    },
                    _ => 'inner: {
                        break 'inner;
                    }
                }
            }
            if current_position != 0{
                current_position -= 1;
            }
            else {
                // prevent underflow by any means
                return Err(format!("Did not parse array correctly"));
            }
        }
        // basic found
        if !sizes.is_empty(){
            sizes.reverse();
            let mut cur_ref = Value::ArrayOf(basic_val.clone().into(), sizes.pop().unwrap());

            for next_size in sizes.iter().rev() {
                cur_ref = Value::ArrayOf(cur_ref.clone().into(), *next_size);
            }
            return Ok(cur_ref);
        }
        else {
            return Ok(basic_val);
        }
    }


    // helper function

    pub fn renderer(tok_list:Vec<tokens>) -> String{
        let mut built = "".to_owned();
        for tok in tok_list{
            match tok {
                tokens::Null => built = built + "",
                tokens::Id(_) => built = built + "id ",
                tokens::Basic(_) => built = built + "basic ",
                tokens::Number(_) => built = built + "num ",
                tokens::Real(_) => built = built + "real ",
                tokens::True => built = built + "true ",
                tokens::False => built = built + "false ",
                tokens::While => built = built + "while ",
                tokens::For => built = built + "for ",
                tokens::Plus => built = built + "+ ",
                tokens::Minus => built = built + "- ",
                tokens::Times => built = built + "* ",
                tokens::Equals => built = built + "== ",
                tokens::Devides => built = built + "/ ",
                tokens::Open => built = built + "( ",
                tokens::Close => built = built + ") ",
                tokens::Stop => built = built + "; ",
                tokens::StartEnclose => built = built + "{ ",
                tokens::EndEnclose => built = built + "} ",
                tokens::Do => built = built + "do ",
                tokens::Les => built = built + "< ",
                tokens::Leq => built = built + "<= ",
                tokens::Gre => built = built + "> ",
                tokens::Geq => built = built + ">= ",
                tokens::If => built = built + "if ",
                tokens::Else => built = built + "if ",
                tokens::Break => built = built + "break ",
                tokens::Assigns => built = built + "= ",
                tokens::And => built = built + "&& ",
                tokens::Or => built = built + "|| ",
                tokens::Neq => built = built + "!= ",
                tokens::ArrayEquationStart => built = built + "[ ",
                tokens::ArrayEquationEnd => built = built + "] ",
                tokens::Not => built = built + "! ",
            }
        }
        return built;
    }
}

fn is_last_char_letter(s: &str) -> bool {
    s.chars()
     .last() // Returns Option<char>
     .map_or(false, |c| c.is_alphanumeric())
     || s.chars().last().eq(&Some('_'))
     || s.chars().last().eq(&Some('.'))
}


#[test]
pub fn test_lexer() {
    let code = "int i;\n\twhile (true)\n\t\ti=i+1;";
    let mut par = Lexer{root_env:Env::new(None)};
    let result = par.custom_lexer(code).unwrap();
    println!("{}", Lexer::renderer(result));
    par.root_env.print_all();
}

use std::fs;
#[test]
pub fn test_lexer_from_file(){
    let contents = fs::read_to_string("TestSuites2/1.txt");
    let code = contents.unwrap();
    let mut par = Lexer{root_env:Env::new(None)};
    let result = par.custom_lexer(&code).unwrap();
    println!("{}", Lexer::renderer(result));
    println!("------------");
    par.root_env.print_all();
}


#[test]
fn true_file_test(){
    let args:Vec<String> = vec!["Something".to_string(), "TestSuites2/fail1.txt".to_string()];
    if !args.is_empty(){
        // something in the argument. expect 1 file arguement
        let file_to_parse = &args[1];
        let file = fs::read_to_string(file_to_parse);
        if file.is_ok(){
            let mut lexer = Lexer{
                root_env: Env::new(None),
            };

            
            let lex_output = lexer.custom_lexer(&file.unwrap());
            if lex_output.is_err(){
                println!("Parsing failed :(");
                return;
            }else {
                println!("{}", Lexer::renderer(lex_output.unwrap()));
                println!("\nParsing completed successfully.\n\nSymbol Table:\n\n----");
                lexer.root_env.print_all();
                return;
            }

        }
        else {
            println!("Invalid file: {:?}", args)
        }
    }
    else {
        println!("no file given")
    }
}

