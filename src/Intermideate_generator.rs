

use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

use super::symbol_table::{Env,Value};
use super::syntax_evaluator::{SyntaxTree,SyntaxTreeNode,IndexBox};
use super::parser::{TreeControler,Rules};
use super::type_checker::size_calculator;

#[derive(Debug)]
enum OperationCode {
    // math
    Add,
    Sub,
    Mul,
    Dev,

    // conditions
    And,
    Or,
    Les,
    Leq,
    Mor,
    Meq,
    Eeq,
    Neq,

    // Data management
    Put,
    Get,

    // exicution order
    Goto,
    IfJump,
}

#[derive(Debug,Clone)]
struct Address {
    index:usize,
    size:usize,
    id: String,
}

struct AddressManager {
    free_list:Vec<(usize,usize)>, // free range in the heap
    heap_end: usize,
    address_list: HashMap<String,Address>,
}
impl AddressManager {
    pub fn new_addr(&mut self, id:String, size:usize) -> Address {
        // check free list for enough space
        for ind in 0..self.free_list.len(){
            let mut range = self.free_list[ind];
            if size <= range.1-range.0 {
                let addr = range.0;
                self.free_list[ind] = (range.0+size, range.1);
                let full_addr = Address {
                    id:id.clone(),
                    size,
                    index: addr
                };
                self.address_list.insert(id, full_addr.clone());
                return full_addr;
            }
        }
        // no space in the free list
        let addr = self.heap_end;
        self.heap_end += size;
        let full_addr = Address {
            id:id.clone(),
            size,
            index: addr
        };
        self.address_list.insert(id, full_addr.clone());
        return full_addr;
    }
    pub fn get_addr_by_id(self, id:String) -> Result<Address,String>{
        return self.address_list.get(&id).ok_or_else(|| format!("could not find id") ).cloned()
    }
}


#[derive(Debug)]
struct Record {
    source1:Address,
    source2:Option<Address>, // some commands dont need a second source
    op_code:OperationCode,
    result:Address
}



struct RecordManager {
    all_records: Vec<Record>,
    tree_controler: TreeControler,
    adddr_manager: AddressManager,
    global_env: Rc<Env>,
    temp_count: i32,
    ast_roots: Vec<SyntaxTree>,
}

impl RecordManager {
    pub fn add_record(&mut self,rec:Record) {
        self.all_records.push(rec);
    }

    pub fn print_records(&self) -> String{
        let mut out = "".to_owned();
        for rec in &self.all_records {
            if rec.source2.is_some(){
               out = out.to_owned() + &format!("{:?} = {:?} {:?} {:?}\n", rec.result,rec.source1, rec.op_code, rec.source2);
            } else {
               out = out.to_owned() + &format!("{:?} = {:?} {:?}\n", rec.result, rec.op_code, rec.source1);
            }
        }
        return out.to_owned();
    }

    pub fn make_temp(&mut self, current_env:&Rc<Env>, typ: Value) -> String{
        let mut tab_borrow = current_env.table.borrow_mut();
        let tmp_key = format!("t{}",self.temp_count);
        tab_borrow.insert(tmp_key.clone(), typ);
        self.temp_count +=1;
        return tmp_key;
    }

    pub fn widen(&self, first:Value, second:Value) -> Result<(Value,Value), String>{
        // this language only has like 3 types this is a very sumple function
        // float > int
        match first {
            Value::AnyFloat | Value::Float(_)  => {
                if !matches!(second, Value::ArrayOf(_,_ ) | Value::Null | Value::AnyBool | Value::Bool(_)){
                    if let Value::Float(_) = second {
                        return Ok((first, second));
                    }
                    if let Value::Int(i) = second {
                        return Ok((first, Value::Float(i as f32)));
                    }
                    return Ok((first, Value::AnyFloat));
                } 
                return Err(format!("Invalid Type Matchup"));
            },
            Value::AnyInt | Value::Int(_)  => {
                if !matches!(second, Value::ArrayOf(_,_ ) | Value::Null | Value::AnyBool | Value::Bool(_)){
                    if let Value::Int(_) | Value::AnyInt = second {
                        return Ok((first, second));
                    }
                    // widen first
                    if let Value::Int(i) = first {
                        return Ok((Value::Float(i as f32), second));
                    }
                    return Ok((Value::AnyFloat, second));
                } 
                return Err(format!("Invalid Type Matchup"));
            }
            _ => {
                return Err(format!("Invalid Type Matchup"));
            }
        }
    }

    fn get_array_metadata(&self, array_id: &str, env: &Rc<Env>) -> Result<(Vec<usize>, usize), String> {        
        let table = size_calculator(env)?;
        
        // Find the record for the array_id
        let array_entry = table.iter()
            .find(|(_, id, _, _, _)| id == array_id)
            .ok_or_else(|| format!("Array '{}' not found in symbol table or size calculation.", array_id))?;
        
        // array_entry is (MemoryType, String, Value, Vec<usize> (dims), usize (total size))
        let dimensions = array_entry.3.clone();
        let base_size = array_entry.4 / dimensions.iter().product::<usize>(); // Calculate size of base element
        
        Ok((dimensions, base_size))
    }

    fn get_address(&self,id:String) -> Address {
        // Unimplemented
        return Address{index:0, id,size:0};
    }

    // Mathmatical code generated elsewhere
    fn generate_assignment_code(&mut self, source:String, sink:String, current_env:Rc<Env>) -> Result<Record,String>{
        let source_type = Env::get(&current_env, source).unwrap();
        let sink_type = Env::get(&current_env, sink).unwrap();

        // type checking and type conversion
        let winden_attempt = self.widen(source_type, sink_type);
        if winden_attempt.is_ok() {
            let (wide_source, wide_sink) = winden_attempt.unwrap();
            let source_tmp = self.make_temp(&current_env, wide_source);
            let sink_tmp = self.make_temp(&current_env, wide_sink);
            let source_addr = self.get_address(source_tmp);
            let sink_addr = self.get_address(sink_tmp);
            
            let top_record = Record {
                source1: source_addr,
                source2: None,
                op_code: OperationCode::Put,
                result: sink_addr,
            };

            return Ok(top_record);
        } else {
            return Err(format!("Widen Failure: {}",winden_attempt.unwrap_err()));
        }
    }

    // recursive code generation
    pub fn generate_code(&self, tree_index:usize, ast_index:usize) -> usize{
        let top_node = self.tree_controler.get_weak(tree_index).unwrap();

        // equation or condition
        if matches!(top_node.rule,Rules::ExprToAssignEquation | Rules::ConditionToObjectHcon){
            // there is an ast about this
            let this_ast = self.ast_roots[ast_index].clone();
            let tree_vec_clone = Rc::clone(&this_ast.tree_vector);
            let tree_vec = tree_vec_clone.borrow();
            let top_ind = this_ast.top_node;
            
            // to be implemented later


            // go to next ast index
            return ast_index+1;
        }



        return ast_index;
    }

    // recursive function that generates ast code.
    pub fn generate_ast_code(&self, tree_vec: &mut RefMut<'_, Vec<Rc<RefCell<SyntaxTreeNode>>>>,tree:SyntaxTree,current_ind:IndexBox) -> Result<Vec<Record>,String>{
        // step 1 recurs into leaves
        let binding = Rc::clone(&tree_vec[current_ind.get()]);
        let this_node = &binding.borrow();
        let children = &this_node.child_nodes;
        let mut child_1_code:Option<Vec<Record>> = None;
        let mut child_2_code:Option<Vec<Record>> = None;
        if children.0.is_some(){child_1_code = Some(self.generate_ast_code(tree_vec, tree.clone(), children.0.clone().unwrap())?);}
        if children.1.is_some(){child_2_code = Some(self.generate_ast_code(tree_vec, tree.clone(), children.1.clone().unwrap())?);}
        



        Ok(Vec::new())
    }
}





#[test]
fn addresses_test_heap_extension_allocation() {
    let mut manager = AddressManager {
        free_list: vec![], // Empty free list
        heap_end: 100,     // Heap starts at index 100
        address_list: HashMap::new(),
    };

    let var_id = "temp_var_x".to_string();
    let size_needed = 8; // Size of a 64-bit value
    let addr = manager.new_addr(var_id.clone(), size_needed);
    println!("Should be 100: {}",addr.index);
    println!("Should be 108: {}",manager.heap_end);
    println!("Should contain key: {}",manager.heap_end);

    let retrieved = manager.get_addr_by_id(var_id).unwrap();
    println!("retreved index should be 100: {}",retrieved.index);
}

#[test]
fn addresses_test_free_list_allocation() {
    let mut manager = AddressManager {
        // Gap from index 200 to 216 (size 16)
        free_list: vec![(200, 216)], 
        heap_end: 500, // Should be ignored by this allocation
        address_list: HashMap::new(),
    };

    let var_id = "free_temp".to_string();
    let size_needed = 4; // Size of a 32-bit value=
    let addr = manager.new_addr(var_id.clone(), size_needed);
    println!("Should be 200: {}",addr.index);
    println!("Should be 500: {}",manager.heap_end);
    println!("Free list should be (204, 216): {:?}",manager.free_list[0]);
}

#[test]
fn instructions_records_test_printing() {
    let addr_manager = AddressManager {
        // Gap from index 200 to 216 (size 16)
        free_list: vec![(200, 216)], 
        heap_end: 500,
        address_list: HashMap::new(),
    };
    let mut rec_manager = RecordManager {
        all_records: Vec::new(),
        tree_controler: TreeControler::new(),
        adddr_manager: addr_manager,
        global_env: Env::new_root(),
        temp_count: 0,
        ast_roots: Vec::new(),
    };
    
    // Define Addresses conceptually for printing (IDs help visualization)
    let addr_t1 = Address { id: "t1".to_string(), size: 4, index: 1 };
    let addr_t2 = Address { id: "t2".to_string(), size: 4, index: 2 };
    let addr_t3 = Address { id: "t3".to_string(), size: 4, index: 3 };
    
    // Binary Record: t3 = t1 + t2
    rec_manager.add_record(Record {
        source1: addr_t1.clone(),
        source2: Some(addr_t2.clone()),
        op_code: OperationCode::Add,
        result: addr_t3.clone(),
    });
    
    // Unary Record (Put/Assignment): t1 = Put t3
    rec_manager.add_record(Record {
        source1: addr_t3.clone(),
        source2: None,
        op_code: OperationCode::Put,
        result: addr_t1.clone(),
    });

    let output = rec_manager.print_records();
    println!("Generated Records:\n{}", output);
}

#[test]
fn instructions_records_test_make_temp_and_print() {
    let addr_manager = AddressManager {
        // Gap from index 200 to 216 (size 16)
        free_list: vec![(200, 216)], 
        heap_end: 500,
        address_list: HashMap::new(),
    };
    let mut rec_manager = RecordManager {
        all_records: Vec::new(),
        tree_controler: TreeControler::new(),
        adddr_manager: addr_manager,
        global_env: Env::new_root(),
        temp_count: 0,
        ast_roots: Vec::new(),
    };
    let current_env = Rc::clone(&rec_manager.global_env);

    // 1. Create temporary variables
    let temp_a_key = rec_manager.make_temp(&current_env, Value::AnyInt); // Should be t0
    let temp_b_key = rec_manager.make_temp(&current_env, Value::AnyFloat); // Should be t1
    
    // 2. Get their conceptual addresses
    let addr_a = Address { id: temp_a_key.clone(), size: 4, index: rec_manager.temp_count as usize - 2 };
    let addr_b = Address { id: temp_b_key.clone(), size: 4, index: rec_manager.temp_count as usize - 1 };

    // 3. Check temp creation and type insertion
    let env_content = current_env.table.borrow();
    println!("Temporary Key t0 (Should be {}): {:?}", temp_a_key, env_content.get(&temp_a_key).unwrap_or(&Value::Null));
    println!("Temporary Key t1 (Should be {}): {:?}", temp_b_key, env_content.get(&temp_b_key).unwrap_or(&Value::Null));
    println!("Temp Count (Should be 2): {}", rec_manager.temp_count);

    // 4. Add a record using the temps (t1 = Put t0)
    rec_manager.add_record(Record {
        source1: addr_a.clone(),
        source2: None,
        op_code: OperationCode::Put,
        result: addr_b.clone(),
    });
    
    let output = rec_manager.print_records();
    println!("\nGenerated Convert Record:\n{}", output);
}

#[test]
fn temp_vars_records_test_printing_complex() {
    let addr_manager = AddressManager {
        // Gap from index 200 to 216 (size 16)
        free_list: vec![(200, 216)], 
        heap_end: 500,
        address_list: HashMap::new(),
    };
    let mut rec_manager = RecordManager {
        all_records: Vec::new(),
        tree_controler: TreeControler::new(),
        adddr_manager: addr_manager,
        global_env: Env::new_root(),
        temp_count: 0,
        ast_roots: Vec::new(),
    };
    
    // Define Addresses conceptually
    let addr_v1 = Address { id: "v1".to_string(), size: 4, index: 10 };
    let addr_v2 = Address { id: "v2".to_string(), size: 4, index: 14 };
    let addr_res = Address { id: "res_r".to_string(), size: 8, index: 20 };
    
    // Record 1: Binary Operation (v1 = v2 * lit_5)
    let addr_lit = Address { id: "lit_5".to_string(), size: 4, index: 100 };
    rec_manager.add_record(Record {
        source1: addr_v2.clone(),
        source2: Some(addr_lit.clone()),
        op_code: OperationCode::Mul,
        result: addr_v1.clone(),
    });

    // Record 2: Unary Operation (res_r = Put v1)
    rec_manager.add_record(Record {
        source1: addr_v1.clone(),
        source2: None,
        op_code: OperationCode::Put,
        result: addr_res.clone(),
    });
    
    // Record 3: Another Binary Operation (v2 = res_r - v1)
    rec_manager.add_record(Record {
        source1: addr_res.clone(),
        source2: Some(addr_v1.clone()),
        op_code: OperationCode::Sub,
        result: addr_v2.clone(),
    });

    let output = rec_manager.print_records();
    println!("Generated Complex Records:\n{}", output);
}

#[test]
fn temp_vars_records_test_make_temp_and_use() {
    println!("\n--- Test 6: RecordManager Make Temp & Use in Code (Binary Add) ---");
    let addr_manager = AddressManager {
        // Gap from index 200 to 216 (size 16)
        free_list: vec![(200, 216)], 
        heap_end: 500,
        address_list: HashMap::new(),
    };
    let mut rec_manager = RecordManager {
        all_records: Vec::new(),
        tree_controler: TreeControler::new(),
        adddr_manager: addr_manager,
        global_env: Env::new_root(),
        temp_count: 0,
        ast_roots: Vec::new(),
    };
    let current_env = Rc::clone(&rec_manager.global_env);

    // 1. Create two temporary variables for sources and one for the result
    let temp_src1 = rec_manager.make_temp(&current_env, Value::Int(10));  // t0
    let temp_src2 = rec_manager.make_temp(&current_env, Value::Int(5));   // t1
    let temp_res = rec_manager.make_temp(&current_env, Value::AnyInt);   // t2
    
    // 2. Report on temp allocation state
    println!("Temp Count (Should be 3): {}", rec_manager.temp_count);
    
    // 3. Define Addresses for the temps
    let addr_src1 = Address { id: temp_src1.clone(), size: 4, index: 10 };
    let addr_src2 = Address { id: temp_src2.clone(), size: 4, index: 14 };
    let addr_res = Address { id: temp_res.clone(), size: 4, index: 18 };

    // 4. Add a record using all three temps: t2 = t0 + t1
    rec_manager.add_record(Record {
        source1: addr_src1.clone(),
        source2: Some(addr_src2.clone()),
        op_code: OperationCode::Add,
        result: addr_res.clone(),
    });
    
    // 5. Print the environment contents (showing t0, t1, t2 types)
    let env_content = current_env.table.borrow();
    println!("Env Contents (t0, t1, t2):\n  t0: {:?}\n  t1: {:?}\n  t2: {:?}", 
        env_content.get(&temp_src1).unwrap_or(&Value::Null),
        env_content.get(&temp_src2).unwrap_or(&Value::Null),
        env_content.get(&temp_res).unwrap_or(&Value::Null)
    );
    
    // 6. Print the generated record
    let output = rec_manager.print_records();
    println!("\nGenerated Binary Record (t2 = t0 + t1):\n{}", output);
}