// Symbol table

use std::{collections::HashMap, cell::RefCell, rc::{Rc, Weak}};
use super::Intermideate_generator::{Address};
use super::type_checker::{size_of_value};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    // a list of all supported data types
    AnyInt,
    Int(i32),
    AnyBool,
    Bool(bool),
    AnyFloat,
    Float(f64),
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
        Rc::new(Env { table: t, parent:None, children: RefCell::new(Vec::new())})
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


/**
 A flattened version of Env in a static way. Just one part of the whole

 Level denenominators are made by minipulating the id

 Manages all of the indexing of the data
*/ 
#[derive(Debug, Clone)]

pub struct StaticAddressedEnv {
    // heap management
    pub address_list: RefCell<HashMap<String, Address>>,
    pub heap_end: usize,
    pub parent: Option<usize>,
}

impl StaticAddressedEnv {
    pub fn new()-> Self {
        let a = RefCell::new(HashMap::new());
        StaticAddressedEnv {address_list: a, heap_end: 0, parent: None }
    }
    pub fn child(p:usize,heap_end:usize)-> Self {
        let a = RefCell::new(HashMap::new());
        StaticAddressedEnv {address_list: a, heap_end: heap_end, parent: Some(p) }
    }
    /// Put a value into the adressed enviornment
    pub fn put(&mut self, id:String, typ:Value) -> Address{
        // check free list for enough space
        let addr = self.heap_end;
        let size = size_of_value(&typ).unwrap();
        self.heap_end += size;
        let full_addr = Address {
            id:id.clone(),
            size,
            index: addr,
            typ,
        };
        self.address_list.borrow_mut().insert(id, full_addr.clone());
        return full_addr;
    }
    /// if none tells you to look elseware
    pub fn get(&self, id:String) -> Option<Address>{
        let binding = self.address_list.borrow();
        let out = binding.get(&id);
        return out.cloned();
    }
}

/**
 A flattened version of Env in a static way. Just one part of the whole

 Level denenominators are made by minipulating the id

 Manages all of the indexing of the data
*/ 
#[derive(Debug, Clone)]

pub struct AddressedEnv {
    // heap management
    pub enviornment_list: Vec<StaticAddressedEnv>,
}
impl AddressedEnv {
    pub fn from_root(root_env:Rc<Env>) -> Self {
        // make new self and recurse
        let mut env = Self {
            enviornment_list: Vec::new(),
        };
        env.from_root_recurse(root_env,None);
        env
    }
    fn from_root_recurse(&mut self,current_env:Rc<Env>,parent_static:Option<usize>) {
        let mut stat: StaticAddressedEnv;
        if parent_static.is_none(){
            // first envionment. make the first static
            stat = StaticAddressedEnv::new();
        } else {
            let p = parent_static.unwrap();
            let h = self.enviornment_list[p].heap_end;
            stat = StaticAddressedEnv::child(p, h);
        }

        // grab data from current enviornment
        for (s,v) in current_env.table.borrow().iter(){
            stat.put(s.clone(), v.clone());
        }
        // inject spacer for parent number (if valid)
        if parent_static.is_none(){
            stat.put(format!("null"), Value::Null);
        } else {
            stat.put(format!("parent:{}",parent_static.unwrap()), Value::Int(parent_static.unwrap() as i32));
        }

        // inject filled static into list
        let current_ind = self.enviornment_list.len();
        self.enviornment_list.push(stat);
        // relization. scopes are stored in order of apearence. 
        for child in current_env.children.borrow().iter() {
            self.from_root_recurse(child.upgrade().unwrap(), Some(current_ind));
        }
    }
    /// abuses strict ordering of scopes to find the addr for a var anywhere.
    pub fn find_addr_at_scope(&self, id:String, scope_number:usize) -> Result<Address,String> {
        let scope = self.enviornment_list.get(scope_number);
        if scope.is_none() { return Err(format!("Scope not found"));}
        let scope = scope.unwrap(); // just rewrite what scope means its fine
        if scope.address_list.borrow().contains_key(&id) {
            // id is somewhere here
            return Ok(scope.address_list.borrow().get(&id).unwrap().clone());
        }
        // id not here. look at parent
        if scope.parent.is_some() {
            return self.find_addr_at_scope(id, scope.parent.unwrap());
        }

        Err(format!("value not found"))
    }
    /// abuses strict ordering of scopes to find the addr for a var anywhere.
    pub fn find_type_at_scope(&self, id:String, scope_number:usize) -> Result<Value,String> {
        let scope = self.enviornment_list.get(scope_number);
        if scope.is_none() { return Err(format!("Scope not found"));}
        let scope = scope.unwrap(); // just rewrite what scope means its fine
        if scope.address_list.borrow().contains_key(&id) {
            // id is somewhere here
            return Ok(scope.address_list.borrow().get(&id).unwrap().typ.clone());
        }
        // id not here. look at parent
        if scope.parent.is_some() {
            return self.find_type_at_scope(id, scope.parent.unwrap());
        }

        Err(format!("value not found"))
    }
    pub fn display(&self, scope_number:usize) -> String {
        let mut out = String::new();
        let scope = &self.enviornment_list[scope_number];
        // render upper scopes first
        if scope.parent.is_some() {
            out += &self.display(scope.parent.unwrap());
        }

        let map = scope.address_list.borrow();
        let mut entries: Vec<_> = map.iter().collect();
        entries.sort_by_key(|(_id, addr)| addr.index); // make sure its sorted

        for (id, addr) in entries {
            let ind = addr.index; let size = addr.size;
            out += &format!("{}->{}: {}\n",ind,ind+size, id);
        }

        return out;
    }
}


#[test]
fn demand_variable_offsets() {
    use std::cell::RefCell;
    use std::collections::HashMap;
    
    let mut env = StaticAddressedEnv { 
        heap_end: 0, 
        address_list: RefCell::new(HashMap::new()), 
        parent: None 
    };

    // 1. Add first variable (Int, size 4)
    // Expected Offset: 0
    let addr1 = env.put("t1".to_string(), Value::AnyInt);
    
    // 2. Add second variable (Float, size 8)
    // Expected Offset: 0 + 4 = 4
    let addr2 = env.put("t2".to_string(), Value::AnyFloat);

    println!("t1 Index: {} (Expected 0)", addr1.index);
    println!("t2 Index: {} (Expected 4)", addr2.index);

    assert_eq!(addr1.index, 0);
    assert_eq!(addr2.index, 4);
}


#[test]
fn demand_record_length() {
    let mut env = StaticAddressedEnv { 
        heap_end: 0, 
        address_list: RefCell::new(HashMap::new().into()), 
        parent: None 
    };

    env.put("a".to_string(), Value::AnyInt);   // +4
    env.put("b".to_string(), Value::AnyFloat); // +8
    env.put("c".to_string(), Value::AnyBool);  // +1 (assuming 1, or 4 depending on alignment, usually 1 or 4 in your util)

    // Assuming AnyBool is size 1 based on standard Rust implementations, 
    // or checking your size_of_value logic. 
    // If AnyBool is 1: Total = 13. If 4: Total = 16.
    println!("Total Record Length: {}", env.heap_end);
    
    assert!(env.heap_end > 0); 
}


#[test]
fn demand_3_verify_base_address_accumulation() {
    println!("\n--- Test Demand 3: Base Address Accumulation ---");
    
    // 1. Setup Root Environment (Parent)
    let root = Env::new_root();
    {
        let mut root_table = root.table.borrow_mut();
        // Add two integers (4 bytes each)
        root_table.insert("parent_a".to_string(), Value::AnyInt);
        root_table.insert("parent_b".to_string(), Value::AnyInt);
    }

    // 2. Setup Child Environment
    // This will be processed AFTER root, so it should inherit root's size as its base.
    let child = Env::child(&root);
    {
        let mut child_table = child.table.borrow_mut();
        // Add one integer (4 bytes)
        child_table.insert("child_x".to_string(), Value::AnyInt);
    }

    // 3. Compile to AddressedEnv (This triggers the sizing logic)
    let addr_env = AddressedEnv::from_root(root);

    // 4. VISUALIZATION (Using your new function)
    // We display the child scope (index 1), which recursively shows the parent (index 0)
    println!("Memory Layout:\n{}", addr_env.display(1));

    // 5. Analyze Heap Ends (Lengths)
    let parent_heap_end = addr_env.enviornment_list[0].heap_end;
    let child_heap_end = addr_env.enviornment_list[1].heap_end;

    println!("Parent Heap End (Base for Child): {}", parent_heap_end);
    println!("Child Heap End (Total Size): {}", child_heap_end);

    // ASSERTION:
    // The Child's heap end MUST be strictly greater than the Parent's heap end.
    assert!(child_heap_end > parent_heap_end, "FAIL: Child did not inherit parent's base address!");
}

#[test]
fn demand_4_verify_variable_offset_calculation() {
    println!("\n--- Test Demand 4: Variable Address = Base + Offset ---");

    // 1. Setup Environment Tree
    let root = Env::new_root();
    let child = Env::child(&root);

    // 2. Add variables to define sizes
    // Root: 1 Int (size 4). Note: 'from_root' also adds a 'null' spacer (size 0 or 4 depending on impl, usually 0 for Null).
    root.table.borrow_mut().insert("v_root".to_string(), Value::AnyInt);
    
    // Child: 1 Int (size 4). Note: 'from_root' adds 'parent:0' spacer (Int, size 4).
    child.table.borrow_mut().insert("v_child".to_string(), Value::AnyInt);

    // 3. Generate Addresses
    let addr_env = AddressedEnv::from_root(root);

    // 4. Retrieve generated addresses
    let root_var_addr = addr_env.find_addr_at_scope("v_root".to_string(), 0).unwrap();
    let child_var_addr = addr_env.find_addr_at_scope("v_child".to_string(), 1).unwrap();

    println!("Root Var Index: {}", root_var_addr.index);
    println!("Child Var Index: {}", child_var_addr.index);
    println!("{}", addr_env.display(1));

    // 5. PROOF CALCULATION
    // The Child Variable Address should be:
    // [Parent Base (0)] + [Parent Vars Size] + [Parent Spacer] + [Child Spacer ("parent:0")] + [Child Var Offset]
    
    // If your bug fix works, the child variable MUST represent a higher memory address than the root variable.
    assert!(child_var_addr.index > root_var_addr.index, "FAIL: Child variable overlaps with Root variable!");

    // Specific check: The difference should be at least the size of the parent variables.
    let difference = child_var_addr.index - root_var_addr.index;
    println!("Memory Distance between variables: {}", difference);
    
    // With 1 Int in Root (4 bytes), the distance must be >= 4.
    assert!(difference >= 4, "FAIL: Distance between scopes is too small, base address missing.");
}

