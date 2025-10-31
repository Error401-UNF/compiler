// Symbol table

use std::{collections::HashMap, rc::Rc};
use super::lexical_analyzer::tokens;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    // a list of all supported data types
    AnyInt,
    Int(i32),
    AnyString,
    String(String),
    AnyBool,
    Bool(bool),
    AnyFloat,
    Float(f32),
    ArrayOf(Rc<Value>,i32),
    Null
}

impl Value {
    pub fn render(&self) -> String{
        match self {
            Value::AnyInt => return format!("int"),
            Value::Int(i) => return format!("{}",i),
            Value::AnyString => return format!("string"),
            Value::String(s) => return format!("{}",s),
            Value::AnyBool => return format!("bool"),
            Value::Bool(b) => return format!("{}",b),
            Value::AnyFloat => return format!("float"),
            Value::Float(f) => return format!("{}", f),
            Value::ArrayOf(v, i) => return format!("{:?}[{}]",v.render(),i),
            Value::Null => return format!(""),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Env {
    table: HashMap<String,Value>,
    prev: Option<Rc<Env>>,
}

impl Env {
    pub fn new( p: Option<Rc<Env>>)-> Self {
        let t = HashMap::new();
        Self { table: t , prev:p}
    }
    pub fn put(&mut self, s:String, sym:Value){
        self.table.insert(s, sym);
    }
    pub fn get(&self, s:String) -> Option<Value>{

        // look locally        &
        if let Some(val) = self.table.get(&s) {
            return Some(val.clone());
        }
        // recurse
        if let Some(prev_env) = &self.prev {
            return <Env as Clone>::clone(&prev_env).get(s); // dont ask me why this works
        }

        None
    }
    pub fn print_all(&self){
        for pairs in self.table.clone() {
            println!("ID: {}", pairs.0, );
        }
        println!("----");
        // recurse
        if let Some(prev_env) = &self.prev {
            <Env as Clone>::clone(&prev_env).print_all();
        }
    }
    pub fn detailed_print_all(&self){
        for pairs in self.table.clone() {
            println!("ID: {}, Value {:?}", pairs.0, pairs.1);
        }
        println!("----");
        // recurse
        if let Some(prev_env) = &self.prev {
            <Env as Clone>::clone(&prev_env).print_all();
        }
    }
}




#[test]
fn symbol_table_print_test(){
    // root
    let mut root = Env::new(None);
    root.put("Str".to_owned(), Value::String("Some random string".to_owned()));
    root.put("Bool".to_owned(), Value::Bool(false));

    // up 1
    let mut up1 = Env::new(Some(Rc::new(root)));
    up1.put("Val".to_owned(), Value::Int(43));

    // up 2
    let mut up2 = Env::new(Some(Rc::new(up1)));
    up2.put("Float".to_owned(), Value::Float(3.14159));

    up2.print_all();
}