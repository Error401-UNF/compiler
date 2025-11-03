// Symbol table

use std::{collections::HashMap, cell::RefCell, rc::{Rc, Weak}};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    // a list of all supported data types
    AnyInt,
    Int(i32),
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
    pub table: RefCell<HashMap<String, Value>>,
    pub parent: Option<Rc<Env>>,
    pub children: RefCell<Vec<Weak<Env>>>,
}

impl Env {
    pub fn new_root()-> Rc<Env> {
        let t = RefCell::new(HashMap::new());
        Rc::new(Env { table: t , parent:None, children: RefCell::new(Vec::new()) })
    }
    pub fn child( parent: &Rc<Env>)-> Rc<Env> {

        let child_env = Env {
            table: RefCell::new(HashMap::new()),
            parent: Some(Rc::clone(&parent)), 
            children: RefCell::new(Vec::new()),
        };

        let child_rc = Rc::new(child_env);
        let weak_child = Rc::downgrade(&child_rc);
        parent.children.borrow_mut().push(weak_child);

        return child_rc;
    }
    

    pub fn put(scope:&mut Rc<Env>, s:String, sym:Value){
        scope.table.borrow_mut().insert(s, sym);
    }

    pub fn get(scope:&Rc<Env>, s:String) -> Option<Value>{

        // look locally
        if let Some(val) = scope.table.borrow().get(&s) {
            return Some(val.clone());
        }
        // recurse
        if let Some(parent_env) = &scope.parent {
            return Env::get(&parent_env, s);
        }
        return None;
    }

    pub fn print_down(scope:&Rc<Env>){

        for (id, _value) in scope.table.borrow().iter() {
            println!("ID: {}", id, );
        }
        println!("----");

        // Recurse downward safely
        for weak_child in scope.children.borrow().iter() {
            if let Some(child_rc) = weak_child.upgrade() {
                Env::print_down(&child_rc); 
            } else {
                println!("[Child scope was dropped]");
            }
        }
    }
    pub fn print_detailed_down(scope: &Rc<Env>, depth: usize) {
        let indent = "  ".repeat(depth);
        println!("{}Scope (Depth {})", indent, depth);

        for (id, value) in scope.table.borrow().iter() {
            println!("{}  ID: {}, Value: {:?}", indent, id, value); 
        }
        println!("{}----", indent);

        // Recurse downward safely
        for weak_child in scope.children.borrow().iter() {
            if let Some(child_rc) = weak_child.upgrade() {
                Env::print_detailed_down(&child_rc, depth + 1); 
            } else {
                println!("{}  [Child scope was dropped]", indent);
            }
        }
    }
}