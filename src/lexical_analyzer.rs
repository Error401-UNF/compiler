// lexical_analyzer.rs
use std::{error::Error, fmt::format, rc::Rc};

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
    Array(Value, i32), // thing stored in array and array of size

    // calling array
    GetValue(Rc<tokens>, Rc<tokens>), // gets value from array with id string (value here so it can be a token)

    // static types
    True,
    False,

    // functions
    While,
    For,
    If,
    Do,
    Break,

    // opperators
    Plus,
    Minus,
    Times,
    Equals,
    Devides,
    Les,
    Leq,
    Gre,
    Geq,

    // space control
    Open,
    Close,
    Stop,
    StartEnclose,
    EndEnclose,
}

// regx for all of the things


pub struct parser {
    pub(crate) root_env: Env
}

impl parser{
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
                    "boolean" => {out.push(tokens::Basic(Value::AnyBool)); code_left = &code_left[segment.len()..]; break;}
                    "float" => {out.push(tokens::Basic(Value::AnyFloat)); code_left = &code_left[segment.len()..]; break;}

                    // absalute values
                    "true" => {out.push(tokens::True); code_left = &code_left[segment.len()..]; break;}
                    "false" => {out.push(tokens::False); code_left = &code_left[segment.len()..]; break;}

                    // functions
                    "while" => {out.push(tokens::While); code_left = &code_left[segment.len()..]; break;}
                    "for" => {out.push(tokens::For); code_left = &code_left[segment.len()..]; break;}
                    "if" => {out.push(tokens::If); code_left = &code_left[segment.len()..]; break;}
                    "do" => {out.push(tokens::Do); code_left = &code_left[segment.len()..]; break;}
                    "break" => {out.push(tokens::Break); code_left = &code_left[segment.len()..]; break;}
                    
                    // opperators
                    ";" => {out.push(tokens::Stop); code_left = &code_left[segment.len()..]; break;}
                    "+" => {out.push(tokens::Plus); code_left = &code_left[segment.len()..]; break;}
                    "-" => {out.push(tokens::Minus); code_left = &code_left[segment.len()..]; break;}
                    "=" => {out.push(tokens::Equals); code_left = &code_left[segment.len()..]; break;}
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

                    x if !is_last_char_letter(x) && x.len() > 1 => {

                        if local_env.get(x[..x.len()-1].to_owned()).is_some(){
                            // known variable
                            out.push(tokens::Id(x[..x.len()-1].to_owned()));
                            code_left = &code_left[segment.len()-1..];
                            break;
                        }


                        // we have a set of something then the full stop or space or end of file. This would be an expression we couldnt parse individually.

                        if let Some(&tokens::Basic(_)) = out.last() { 
                            // check for array
                            // check for array (no id attached yet)
                            if x.chars().nth(0).eq(&Some('[')){
                                // this is an array block
                                let num_string = &x[1..x.len()-1]; // isolates number from brackets and the stop symbol ";" or " "
                                let i = num_string.parse::<i32>();
                                if i.is_ok(){
                                    let last_token = out.last().unwrap();
                                    match last_token {
                                        tokens::Basic(v) => {
                                        out.push(tokens::Array(v.clone(),i.unwrap()));
                                        code_left = &code_left[segment.len()..];
                                        if x.chars().last().eq(&Some(';')){ out.push(tokens::Stop);}
                                        break;
                                        },
                                        _ => {
                                            println!("Array error. got {}", segment);
                                            code_left = &code_left[segment.len()..];
                                            break;
                                        },// error ???
                                    }
                                }
                                else {
                                    println!("Array error. got {}", segment);
                                    code_left = &code_left[segment.len()..];
                                    break;
                                }
                            }

                            // default to id
                            // new definition 
                            let last_token = out.last().unwrap();
                            match last_token {
                                tokens::Basic(v) => {
                                    local_env.put(x[..x.len()-1].to_owned(), v.clone());
                                    out.push(tokens::Id(x[..x.len()-1].to_owned()));
                                    // full stop afterwards?
                                    if x.chars().last().eq(&Some(';')){
                                        out.push(tokens::Stop);
                                        code_left = &code_left[segment.len()..];
                                        break;
                                    } else{ // dosnt take into account = segment yet
                                        println!("Encounterd a weird space {}",x);
                                        code_left = &code_left[segment.len()..];
                                        break;
                                    }
                                }
                                _ => {
                                    println!("Token error. got {}", segment);
                                    code_left = &code_left[segment.len()..];
                                    break;
                                }

                            }
                            
                        }
                        
                        // if the last thing was an array. This must be an id
                        if let Some(&tokens::Array(_,_)) = out.last() { 
                            let last_token = out.last().unwrap();
                            match last_token {
                                tokens::Array(v,_) => {
                                local_env.put(x[..x.len()-1].to_owned(), v.clone());
                                out.push(tokens::Id(x[..x.len()-1].to_owned()));
                                code_left = &code_left[segment.len()..];
                                if x.chars().last().eq(&Some(';')){ out.push(tokens::Stop);}
                                break;
                                },
                                _ => {
                                    println!("Array error. got {}", segment);
                                    code_left = &code_left[segment.len()..];
                                    break;
                                },// error ???
                            }
                        }

                        // if last thing was an id check for array?
                        if let Some(&tokens::Id(_)) = out.last() { 
                            if Some('[') == x.chars().nth(0) && Some(']') == x.chars().nth(x.len()-1){
                                // surrunded by []'s
                                let last_token = out.last().unwrap();
                                match last_token {
                                    tokens::Id(v) => {
                                    // determin if the thing inside is a id or a number
                                    if local_env.get(x[1..x.len()-1].to_owned()).is_some(){
                                        out.push(tokens::GetValue(tokens::Id(v.to_string()).into(), tokens::Id(x[1..x.len()-1].to_owned()).into()));
                                    }
                                    else {
                                        let i:Result<i32, _> = x[1..x.len()-1].parse();
                                        if i.is_ok(){
                                            out.push(tokens::GetValue(tokens::Id(v.to_string()).into(), tokens::Number(i.unwrap()).into()));
                                        } else {
                                            println!("Parse of array get val Wrong, {}", segment)
                                        }
                                    }
                                    code_left = &code_left[segment.len()..];
                                    if x.chars().last().eq(&Some(';')){ out.push(tokens::Stop);}
                                    break;
                                    },
                                    _ => {
                                        println!("Array error. got {}", segment);
                                        code_left = &code_left[segment.len()..];
                                        break;
                                    },// error ???
                                }
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
                            break;
                        } else if r.is_ok(){
                            out.push(tokens::Real(r.unwrap()));
                            code_left = &code_left[segment.len()..];
                            if x.chars().last().eq(&Some(';')){ out.push(tokens::Stop);}
                            break;
                        }
                        return Err(format!("problem with segment {:?}: {:?}",segment, out));
                    }

                    x if x.chars().last().eq(&Some('[')) && x.len() != 1 => {
                        // should be a known array variable
                        if local_env.get(x[..x.len()-1].to_owned()).is_some(){
                            // known variable
                            out.push(tokens::Id(x.to_owned()));
                            code_left = &code_left[segment.len()-1..];
                            break;
                        }
                        else {
                            println!("Array of non-known id");
                            code_left = &code_left[segment.len()-1..];
                            break;
                        }
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
                tokens::Equals => built = built + "= ",
                tokens::Devides => built = built + "/ ",
                tokens::Open => built = built + "( ",
                tokens::Close => built = built + ") ",
                tokens::Stop => built = built + "; ",
                tokens::StartEnclose => built = built + "{ ",
                tokens::EndEnclose => built = built + "} ",
                tokens::Array(_value, _n) =>  built = built + "[ num ] ",
                tokens::GetValue(_, to_token) => {let rec = Self::renderer(vec![Rc::unwrap_or_clone(to_token)]); built = built + &format!("[ {} ] ", &rec[..rec.len()-1]);},
                tokens::Do => built = built + "do ",
                tokens::Les => built = built + "< ",
                tokens::Leq => built = built + "<= ",
                tokens::Gre => built = built + "> ",
                tokens::Geq => built = built + ">= ",
                tokens::If => built = built + "if ",
                tokens::Break => built = built + "break ",
            }
        }
        return built;
    }
}

fn is_last_char_letter(s: &str) -> bool {
    s.chars()
     .last() // Returns Option<char>
     .map_or(false, |c| c.is_alphanumeric()) 
    || s.chars().last().eq(&Some('['))
}

#[test]
pub fn test_lexer() {
    let code = "int i;\n\twhile (true)\n\t\ti=i+1;";
    let mut par = parser{root_env:Env::new(None)};
    let result = par.custom_lexer(code).unwrap();
    println!("{}", parser::renderer(result));
    par.root_env.print_all();
}

use std::fs;
#[test]
pub fn test_lexer_from_file(){
    let contents = fs::read_to_string("TestSuites2/1.txt");
    let code = contents.unwrap();
    let mut par = parser{root_env:Env::new(None)};
    let result = par.custom_lexer(&code).unwrap();
    println!("{}", parser::renderer(result));
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
            let mut parser = parser{
                root_env: Env::new(None),
            };

            
            let lex_output = parser.custom_lexer(&file.unwrap());
            if lex_output.is_err(){
                println!("Parsing failed :(");
                return;
            }else {
                println!("{}", parser::renderer(lex_output.unwrap()));
                println!("\nParsing completed successfully.\n\nSymbol Table:\n\n----");
                parser.root_env.print_all();
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

