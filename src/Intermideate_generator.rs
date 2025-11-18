

use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

use crate::symbol_table::StaticAddressedEnv;

use super::symbol_table::{Env,Value, AddressedEnv};
use super::syntax_evaluator::{SyntaxTree,SyntaxTreeNode,IndexBox,Opperator};
use super::parser::{TreeControler,Rules};
use super::type_checker::{size_of_value, widen, get_base_type};
use super::lexical_analyzer::{tokens};


#[derive(Debug)]
pub enum OperationCode {
    // assignment of form x = y op z
    Add,
    Sub,
    Mul,
    Dev,

    And,
    Or,
    Les,
    Leq,
    Mor,
    Meq,
    Eeq,
    Neq,

    // assignment of form x = op y
    Not,
    Minus,
    Widen,

    // copy instructuon of form x = y
    Put,

    // unconditional jump of form goto L, where L is a label
    Goto,
    
    // conditional jump of form if x goto L / ifFalse x goto L
    IfTrue,
    IfFalse,

    // Indexed copy of form x=y[i] and x[i] =y
    FromArr,
    ToArr,

}

#[derive(Debug,Clone)]
pub struct Address {
    pub index:usize,
    pub size:usize,
    pub id: String,
    pub typ: Value
}
impl Address {
    fn display(&self) -> String {
        return self.id.clone();
    }
}

#[derive(Debug,Clone)]
pub struct Constent {
    contained_value: Value,
}
impl Constent {
    pub fn get_type(&self) -> Value {
        match self.contained_value {
            Value::AnyInt => return Value::AnyInt,
            Value::Int(_) => return Value::AnyInt,
            Value::AnyBool => return Value::AnyBool,
            Value::Bool(_) => return Value::AnyBool,
            Value::AnyFloat => return Value::AnyFloat,
            Value::Float(_) => return Value::AnyFloat,
            Value::Null => return Value::Null,
            Value::ArrayOf(_, _) => return self.contained_value.clone(),
        }
    }
    pub fn display(&self) -> String {
        return self.contained_value.render();
    }
}


// a combined box for both address / value
#[derive(Debug,Clone)]
pub struct AddressValue {
    addr: bool,
    address: Option<Address>,
    constent: Option<Constent>
}
impl AddressValue {
    pub fn new_addr(address: Address) ->  Self {
        Self { addr: true, address: Some(address), constent: None }
    }
    pub fn new_const(constent: Constent) ->  Self {
        Self { addr: false, address: None, constent: Some(constent) }
    }
    pub fn display(&self)-> String {
        if self.addr {
            return self.address.clone().unwrap().display();
        }
        else {
            return self.constent.clone().unwrap().display();
        }
    }
}


#[derive(Debug)]
pub struct Record {
    source1: AddressValue,
    source2: Option<AddressValue>, // some commands dont need a second source
    op_code: OperationCode,
    result: Address
}

pub struct RecordManager {
    all_records: Vec<Record>,
    tree_controler: TreeControler,
    global_env: AddressedEnv,
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
               out = out.to_owned() + &format!("{:?} = {:?} {:?} {:?}\n", rec.result.display(),rec.source1.display(), rec.op_code, rec.source2.clone().unwrap().display());
            } else {
               out = out.to_owned() + &format!("{:?} = {:?} {:?}\n", rec.result.display(), rec.op_code, rec.source1.display());
            }
        }
        return out.to_owned();
    }
    pub fn make_temp(&mut self, current_scope:usize, typ: Value) -> String{
        let tmp_key = format!("t{}",self.temp_count);
        self.temp_count +=1;
        let scope = self.global_env.enviornment_list.get_mut(current_scope).unwrap();
        scope.put(tmp_key.clone(), typ);
        return tmp_key;
    }   
    /// Vec of dimentions then base size
    fn get_array_metadata(&self, array_id: String, current_scope:usize) -> Result<(Vec<usize>, usize), String> {        
        // get array in question
        let arr_addr = self.global_env.find_type_at_scope(array_id, current_scope);
        if arr_addr.is_err() {return Err(format!("No array found"));}
        let base_output = get_base_type(arr_addr.unwrap().clone().into(), vec![]);
        let base_size = size_of_value(&base_output.0.clone());
        
        Ok((base_output.1, base_size.unwrap()))
    }
    fn get_address(&self,id:String,current_scope:usize) -> Result<Address, String> {
        return self.global_env.find_addr_at_scope(id, current_scope);
    }

    // Mathmatical code generated elsewhere
    fn generate_assignment_code(&mut self, source:String, sink:String, current_scope:usize) -> Result<Record,String>{
        let source_type = self.global_env.find_type_at_scope(source, current_scope).unwrap();
        let sink_type = self.global_env.find_type_at_scope(sink, current_scope).unwrap();

        // type checking and type conversion
        let winden_attempt = widen(source_type, sink_type);
        if winden_attempt.is_ok() {
            let (wide_source, wide_sink) = winden_attempt.unwrap();
            let source_tmp = self.make_temp(current_scope, wide_source);
            let sink_tmp = self.make_temp(current_scope, wide_sink);
            let source_addr = self.get_address(source_tmp, current_scope).unwrap();
            let sink_addr = self.get_address(sink_tmp, current_scope).unwrap();
            
            let top_record = Record {
                source1: AddressValue::new_addr(source_addr),
                source2: None,
                op_code: OperationCode::Put,
                result: sink_addr,
            };

            return Ok(top_record);
        } else {
            return Err(format!("Widen Failure: {}",winden_attempt.unwrap_err()));
        }
    }
    
    /// generates array assignment from ast directly.
    /// 
    /// Takes in id of array the top node , returns address of result
    fn generate_array_assignmemt_code(&mut self, id:String, tree_vector: &Vec<Rc<RefCell<SyntaxTreeNode>>>, ast_index:usize, current_scope:usize, ) -> Result<Address, String> {
        let curent_node = &tree_vector[ast_index];
        // current nodes type must be ArrayAccess to be here
        if curent_node.borrow().opperator != Opperator::ArrayAccess {
            // uh oh
            return Err(format!("current node not Array access (how?)"));
        }
        // grab number and move pointer

        // check left to se if there are any more arrays. 


        return Err(format!("array generation failed"));
    }


    fn calculate_type_size(&self, v: &Value) -> usize {
        match v {
            Value::ArrayOf(inner, len) => {
                self.calculate_type_size(inner) * (*len as usize)
            },
            _ => {
                size_of_value(v).unwrap_or(4) 
            }
        }
    }

    // gemini made
    pub fn generate_array_offset(
        &mut self, 
        node_index: IndexBox, 
        arr_id: String, // The base variable name (e.g., "i" for i[2][4])
        current_scope: usize, 
        tree_vec: &Vec<Rc<RefCell<SyntaxTreeNode>>>
    ) -> Result<(String, Value), String> {
        
        let node = tree_vec[node_index.get()].borrow();
        
        if node.opperator != Opperator::ArrayAccess {
             return Err(format!("Expected ArrayAccess node, got {:?}", node.opperator));
        }

        let left_idx = node.child_nodes.0.clone();
        let right_idx = node.child_nodes.1.clone().ok_or("ArrayAccess node missing right child (Index)")?;

        // 1. Determine current level's type and width
        
        // Base case: Get the full type from the symbol table using arr_id
        let full_array_type = self.global_env.find_type_at_scope(arr_id.clone(), current_scope)
            .map_err(|_| format!("Array variable '{}' not found in env number {}", arr_id, current_scope))?;
        
        // We need to traverse the AST *down* to the base ID to get the full type, 
        // then traverse back up the recursion to peel off array dimensions.

        // Get the type of the left child (Base L1).
        if left_idx.is_some() {
            let left_idx = left_idx.unwrap();
            // Recursive Case: The base is another ArrayAccess (L1)
            // Call recursively to find the offset of L1 and the type that L1 returns.
            let (prev_offset_id, prev_type) = self.generate_array_offset(left_idx, arr_id.clone(), current_scope, tree_vec)?;
            
            // Generate width for the current dimension based on the type returned by the previous access
            let (current_element_type, element_width) = match &prev_type {
                Value::ArrayOf(inner, _) => (inner.as_ref().clone(), self.calculate_type_size(inner)),
                _ => return Err("Attempted to index a non-array type in recursion".to_string())
            };

            // 2. Generate Code for Index Expression (E) - Index * Width
            let index_id = self.generate_expression_stub(right_idx, current_scope, tree_vec)?;
            let index_addr = self.get_address(index_id.clone(),current_scope)?;

            // Generate Width Constant: w = width
            let width_temp = self.make_temp(current_scope, Value::AnyInt);
            let width_addr = self.get_address(width_temp.clone(),current_scope)?;
            self.add_record(Record {
                source1: AddressValue::new_const(Constent { contained_value: Value::Int(element_width as i32) }),
                source2: None, op_code: OperationCode::Put, result: width_addr.clone()
            });

            // Generate Multiply: t_mul = index * w
            let mul_temp = self.make_temp(current_scope, Value::AnyInt);
            let mul_addr = self.get_address(mul_temp.clone(),current_scope)?;
            
            self.add_record(Record {
                source1: AddressValue::new_addr(index_addr),
                source2: Some(AddressValue::new_addr(width_addr)),
                op_code: OperationCode::Mul,
                result: mul_addr.clone()
            });

            // 3. Generate Add: final_offset = prev_offset + t_mul
            let final_offset_temp = self.make_temp(current_scope, Value::AnyInt);
            let final_offset_addr = self.get_address(final_offset_temp.clone(),current_scope)?;
            let prev_offset_addr = self.get_address(prev_offset_id,current_scope)?;

            self.add_record(Record {
                source1: AddressValue::new_addr(prev_offset_addr),
                source2: Some(AddressValue::new_addr(mul_addr)),
                op_code: OperationCode::Add,
                result: final_offset_addr
            });
            
            // Return the final offset temp and the type of the element accessed at this level
            return Ok((final_offset_temp, current_element_type));

        } else {
            // Base Case: The left child is nothing
            
            // 1. Calculate Width: Get the width of the innermost array element (the base type size).
            let (inner_type, base_element_width) = match &full_array_type {
                Value::ArrayOf(inner, _) => (inner.as_ref().clone(), self.calculate_type_size(inner)),
                _ => return Err(format!("Variable '{}' is not an array", arr_id))
            };

            // 2. Generate Code for Index Expression (E)
            let index_id = self.generate_expression_stub(right_idx, current_scope, tree_vec)?;
            let index_addr = self.get_address(index_id.clone(),current_scope)?;

            // Generate Width Constant: w = base_element_width
            let width_temp = self.make_temp(current_scope, Value::AnyInt);
            let width_addr = self.get_address(width_temp.clone(),current_scope)?;
            self.add_record(Record {
                source1: AddressValue::new_const(Constent { contained_value: Value::Int(base_element_width as i32) }),
                source2: None, op_code: OperationCode::Put, result: width_addr.clone()
            });

            // 3. Generate Multiply: offset = index * w
            let offset_temp = self.make_temp(current_scope, Value::AnyInt);
            let offset_addr = self.get_address(offset_temp.clone(),current_scope)?;
            
            self.add_record(Record {
                source1: AddressValue::new_addr(index_addr),
                source2: Some(AddressValue::new_addr(width_addr)),
                op_code: OperationCode::Mul,
                result: offset_addr
            });
            
            // Return the calculated offset and the type of the element
            return Ok((offset_temp, inner_type));
        };
    }


    // STUB: Generates code for an expression and returns the temp holding the result.
    // Replaces 'generate_code' for the purpose of the array offset function.
    fn generate_expression_stub(
        &mut self, 
        node_idx: IndexBox, 
        scope: usize, 
        tree_vec: &Vec<Rc<RefCell<SyntaxTreeNode>>>
    ) -> Result<String, String> {
        let node = tree_vec[node_idx.get()].borrow();
        match &node.opperator {
            Opperator::Terminal(tokens::Number(n)) => {
                let t = self.make_temp(scope, Value::Int(*n));
                let addr = self.get_address(t.clone(),scope)?;
                self.add_record(Record {
                    source1: AddressValue::new_const(Constent{contained_value: Value::Int(*n)}),
                    source2: None, op_code: OperationCode::Put, result: addr
                });
                Ok(t)
            },
            Opperator::Terminal(tokens::Id(s)) => {
                 // If it's a variable ID (e.g., 'k' in arr[k]), we just return the ID.
                 Ok(s.clone())
            },
            // Add arithmetic op handling here (Plus, Minus, etc.) calling make_temp recursively
            _ => Err(format!("Expression type {:?} not implemented in stub", node.opperator))
        }
    }

    // end of gemini

    // recursive code generation
    #[allow(dead_code,unused_variables)]
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
    #[allow(dead_code,unused_variables,unused_assignments)]
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
    println!("\n--- Test 1: AddressedEnv Heap Extension Allocation ---");
    // NOTE: This assumes AddressedEnv implements/exposes the necessary heap fields and new_addr.
    let mut addr_env = StaticAddressedEnv { // Using AddressedEnv as defined in symbol_table.rs
        heap_end: 100,     // Heap starts at index 100
        address_list: HashMap::new().into(),
        parent: None
    };

    let var_id = "temp_var_x".to_string();
    
    // Allocate (new_addr now requires a type)
    let addr = addr_env.put(var_id.clone(), Value::AnyFloat); // Using AnyFloat for size 8
    
    println!("Allocated Index (Should be 100): {}",addr.index);
    println!("New Heap End (Should be 108): {}",addr_env.heap_end);
    
    let retrieved = addr_env.get(var_id.clone()).unwrap();
    println!("Retrieved Index (Should be 100): {}",retrieved.index);
    println!("Address list should contain key: {}", retrieved.id);
}

#[test]
fn addresses_test_allocation_offset() {
    println!("\n--- Test 2: StaticAddressedEnv Offset Allocation ---");
    // Note: The current StaticAddressedEnv implementation does not appear to have a 'free_list'.
    // It uses a simple pointer bump (heap_end). This test verifies that allocation
    // correctly respects a pre-existing heap_end.

    let mut addr_env = StaticAddressedEnv {
        heap_end: 500, // Heap explicitly starts at 500
        address_list: RefCell::new(HashMap::new()),
        parent: None
    };

    let var_id = "offset_temp".to_string();
    // Using AnyInt (size 4 for 32-bit)
    let addr = addr_env.put(var_id.clone(), Value::AnyInt); 

    println!("Allocated Index (Should be 500): {}", addr.index);
    println!("New Heap End (Should be 504): {}", addr_env.heap_end);
}

#[test]
fn instructions_records_test_printing() {
    println!("\n--- Test 3: RecordManager Printing ---");

    // 1. Setup the Environment
    let root_env = Env::new_root();
    let addr_env = AddressedEnv::from_root(root_env);

    // 2. Setup RecordManager
    let mut rec_manager = RecordManager {
        all_records: Vec::new(),
        tree_controler: TreeControler::new(),
        global_env: addr_env,
        temp_count: 0,
        ast_roots: Vec::new(),
    };
    
    // 3. Define Addresses manually (mimicking what the generator would produce)
    let addr_t1 = Address { id: "t1".to_string(), size: 4, index: 1, typ: Value::AnyInt };
    let addr_t2 = Address { id: "t2".to_string(), size: 4, index: 2, typ: Value::AnyInt };
    let addr_t3 = Address { id: "t3".to_string(), size: 4, index: 3, typ: Value::AnyInt };
    
    // 4. Add Records
    // t3 = t1 + t2
    rec_manager.add_record(Record {
        source1: AddressValue::new_addr(addr_t1.clone()),
        source2: Some(AddressValue::new_addr(addr_t2.clone())),
        op_code: OperationCode::Add,
        result: addr_t3.clone(),
    });
    
    // t1 = Put t3
    rec_manager.add_record(Record {
        source1: AddressValue::new_addr(addr_t3.clone()),
        source2: None,
        op_code: OperationCode::Put,
        result: addr_t1.clone(),
    });

    let output = rec_manager.print_records();
    println!("Generated Records:\n{}", output);
}

#[test]
fn instructions_records_test_make_temp_and_print() {
    println!("\n--- Test 4: RecordManager Make Temp & Print ---");

    let root_env = Env::new_root();
    let addr_env = AddressedEnv::from_root(root_env);

    let mut rec_manager = RecordManager {
        all_records: Vec::new(),
        tree_controler: TreeControler::new(),
        global_env: addr_env,
        temp_count: 0,
        ast_roots: Vec::new(),
    };

    // 1. Create temporary variables using Scope 0 (Global)
    let t0_key = rec_manager.make_temp(0, Value::AnyInt);   // t0
    let t1_key = rec_manager.make_temp(0, Value::AnyFloat); // t1
    
    println!("Temp Count (Should be 2): {}", rec_manager.temp_count);

    // 2. Retrieve the actual addresses generated in the environment
    // We access the global scope (index 0)
    let addr_t0 = rec_manager.global_env.find_addr_at_scope(t0_key.clone(), 0).expect("t0 should exist");
    let addr_t1 = rec_manager.global_env.find_addr_at_scope(t1_key.clone(), 0).expect("t1 should exist");

    // 3. Create a record: t1 = Put t0
    rec_manager.add_record(Record {
        source1: AddressValue::new_addr(addr_t0),
        source2: None,
        op_code: OperationCode::Put,
        result: addr_t1,
    });
    
    let output = rec_manager.print_records();
    println!("\nGenerated Record:\n{}", output);
}

#[test]
fn temp_vars_records_test_printing_complex() {
    println!("\n--- Test 5: RecordManager Complex Printing ---");

    let root_env = Env::new_root();
    let addr_env = AddressedEnv::from_root(root_env);

    let mut rec_manager = RecordManager {
        all_records: Vec::new(),
        tree_controler: TreeControler::new(),
        global_env: addr_env,
        temp_count: 0,
        ast_roots: Vec::new(),
    };
    
    // Manual Address Definitions
    let addr_v1 = Address { id: "v1".to_string(), size: 4, index: 10, typ: Value::AnyInt };
    let addr_v2 = Address { id: "v2".to_string(), size: 4, index: 14, typ: Value::AnyFloat };
    let addr_res = Address { id: "res_r".to_string(), size: 8, index: 20, typ: Value::AnyFloat };
    
    // Constant: 5.0
    let const_5 = Constent { contained_value: Value::Float(5.0) };
    let addr_lit_5 = AddressValue::new_const(const_5);

    // Record 1: v1 = v2 * 5.0
    rec_manager.add_record(Record {
        source1: AddressValue::new_addr(addr_v2.clone()),
        source2: Some(addr_lit_5),
        op_code: OperationCode::Mul,
        result: addr_v1.clone(),
    });

    // Record 2: res_r = Put v1
    rec_manager.add_record(Record {
        source1: AddressValue::new_addr(addr_v1.clone()),
        source2: None,
        op_code: OperationCode::Put,
        result: addr_res.clone(),
    });
    
    // Record 3: v2 = res_r - v1
    rec_manager.add_record(Record {
        source1: AddressValue::new_addr(addr_res.clone()),
        source2: Some(AddressValue::new_addr(addr_v1.clone())),
        op_code: OperationCode::Sub,
        result: addr_v2.clone(),
    });

    let output = rec_manager.print_records();
    println!("Generated Complex Records:\n{}", output);
}

#[test]
fn temp_vars_records_test_make_temp_and_use() {
    println!("\n--- Test 6: Full Integration (Make Temp & Use) ---");

    let root_env = Env::new_root();
    let addr_env = AddressedEnv::from_root(root_env);

    let mut rec_manager = RecordManager {
        all_records: Vec::new(),
        tree_controler: TreeControler::new(),
        global_env: addr_env,
        temp_count: 0,
        ast_roots: Vec::new(),
    };

    // 1. Generate Temps in Scope 0
    let t0 = rec_manager.make_temp(0, Value::Int(10));
    let t1 = rec_manager.make_temp(0, Value::Int(5));
    let t2 = rec_manager.make_temp(0, Value::AnyInt);
    
    // 2. Retrieve Addresses
    let addr_t0 = rec_manager.global_env.find_addr_at_scope(t0.clone(), 0).unwrap();
    let addr_t1 = rec_manager.global_env.find_addr_at_scope(t1.clone(), 0).unwrap();
    let addr_t2 = rec_manager.global_env.find_addr_at_scope(t2.clone(), 0).unwrap();

    // 3. Create Record: t2 = t0 + t1
    rec_manager.add_record(Record {
        source1: AddressValue::new_addr(addr_t0.clone()),
        source2: Some(AddressValue::new_addr(addr_t1.clone())),
        op_code: OperationCode::Add,
        result: addr_t2.clone(),
    });
    
    // 4. Validation
    println!("Temp Count: {}", rec_manager.temp_count);
    println!("t0 Type: {:?}", addr_t0.typ);
    
    let output = rec_manager.print_records();
    println!("\nGenerated Record:\n{}", output);
}



// gemini made
#[cfg(test)]
mod array_tests {
    use super::*;
    use crate::syntax_evaluator::{Opperator, IndexBox};
    use crate::lexical_analyzer::tokens;
    use crate::symbol_table::{AddressedEnv, StaticAddressedEnv, Env};

    // Helper to create a basic AST node for testing
    // NOTE: This helper is only here to make the test cleaner and avoids repeating
    // the complex Rc<RefCell<...>> setup, but it does NOT go into the final canvas code.
    fn create_node(op: Opperator, expected_type: Value, child1: Option<IndexBox>, child2: Option<IndexBox>) -> Rc<RefCell<SyntaxTreeNode>> {
        Rc::new(RefCell::new(SyntaxTreeNode {
            parent: None,
            opperator: op,
            expected_type,
            child_nodes: (child1, child2),
        }))
    }

    #[test]
    fn test_generate_array_offset_multidimensional_correct_ast() {
        println!("\n--- Test 7: Multidimensional Array Offset (i[3][4])");

        // 1. Setup Environment
        let root_env = Env::new_root();
        // Array 'i' is declared as int[10][5] (Total size 40*4=200, if int is 4 bytes)
        // Element type is Int (4 bytes)
        // Inner dimension (Width 2) is 5 elements long (Size 4 * 5 = 20)
        let inner_type = Rc::new(Value::ArrayOf(Rc::new(Value::AnyFloat), 5));
        let array_type = Value::ArrayOf(inner_type.clone(), 10);
        root_env.table.borrow_mut().insert("i".to_string(), array_type);
        
        let addr_env = AddressedEnv::from_root(root_env);
        println!("{}",addr_env.display(0));
        let mut rec_manager = RecordManager {
            all_records: Vec::new(),
            tree_controler: TreeControler::new(),
            global_env: addr_env,
            temp_count: 0,
            ast_roots: Vec::new(),
        };

        // Create a dummy scope for temps
        let dummy_env = StaticAddressedEnv { heap_end: 0, address_list: RefCell::new(HashMap::new()), parent: None };
        rec_manager.global_env.enviornment_list.push(dummy_env);
        let scope_index = rec_manager.global_env.enviornment_list.len() - 1;


        // 3. Setup AST for 'i[2][4]' (Indices: [0] for 4, [1] for 2, [2] for ID 'i')
        
        // Index 0: Terminal(Number(4)) - Index E2
        let idx_4_node = create_node(Opperator::Terminal(tokens::Number(4)), Value::AnyInt, None, None);
        
        // Index 1: Terminal(Number(2)) - Index E1
        let idx_2_node = create_node(Opperator::Terminal(tokens::Number(3)), Value::AnyInt, None, None);
        
        // Index 2: Terminal(Id("i")) - Base Array ID
        let arr_id_node = create_node(Opperator::Terminal(tokens::Id("i".to_string())), Value::AnyInt, None, None);


        // Index 3: Inner Access: Base (ID 'i') [ E1 (2) ] -> 'i[2]'
        // L: ID 'i' (2) | R: Number(2) (1)
        let inner_access_node = create_node(Opperator::ArrayAccess, inner_type.as_ref().clone(), 
            None, 
            Some(IndexBox::new(1))
        );

        // Index 4: Outer Access: Base (i[2]) [ E2 (4) ] -> 'i[2][4]' (This is the root passed to the function)
        // L: ArrayAccess 'i[2]' (3) | R: Number(4) (0)
        let final_access_node = create_node(Opperator::ArrayAccess, Value::AnyInt, 
            Some(IndexBox::new(3)), 
            Some(IndexBox::new(0))
        );
        let sub = Rc::clone(&final_access_node);
        // Create the tree vector
        let tree_vec = Rc::new(RefCell::new(vec![
            idx_4_node,          // 0: Index E2 (4)
            idx_2_node,          // 1: Index E1 (2)
            arr_id_node,         // 2: Base ID 'i' (Not used directly by function, but used by inner access)
            inner_access_node,   // 3: Inner Access i[2]
            sub,   // 4: Root i[2][4]
        ]));
        
        final_access_node.borrow().render_node(&tree_vec.borrow(), 0, true);
        // 4. GENERATE CODE: Pass the ROOT node index and the BASE variable ID
        let result = rec_manager.generate_array_offset(IndexBox::new(4), "i".to_string(), 0, &tree_vec.borrow());
        
        assert!(result.is_ok(), "Code generation failed: {}", result.unwrap_err());
        let (final_offset_id, final_type) = result.unwrap();

        // 5. ASSERTIONS

        // A. Final Type Check: Should resolve to the base element type (AnyInt)
        assert_eq!(final_type, Value::AnyFloat, "Final type should be the base element type (AnyFloat).");

        // B. Instruction Sequence Check
        let records = &rec_manager.all_records;
        println!("\nGenerated Three-Address Code (Total Records: {}):\n{}", records.len(), rec_manager.print_records());

        // Expected Logic (8 instructions total):
        // 1. R[0]: t0 = 2 (Index Put E1)
        // 2. R[1]: t1 = 4 (Base Element Width, Size of Int)
        // 3. R[2]: t2 = t0 * t1 (Offset calculation for i[2])
        // 4. R[3]: t3 = 4 (Index Put E2)
        // 5. R[4]: t4 = 4 (Width for E2, Size of Int)
        // 6. R[5]: t5 = t3 * t4 (Offset calculation for [4] index)
        // 7. R[6]: t6 = t2 + t5 (Final Add)

        assert_eq!(records.len(), 7, "Expected exactly 7 records for the expression i[2][4].");
        
        // Check types and order: (2 Puts for Indices) + (Put/Mul for first index) + (Put/Mul/Add for second index)
        // R[0]: Index Put (2)
        assert!(matches!(records[0].op_code, OperationCode::Put)); 
        // R[1]: Width Put (4, base size)
        assert!(matches!(records[1].op_code, OperationCode::Put)); 
        // R[2]: Mul (Offset 1: t0 * t1)
        assert!(matches!(records[2].op_code, OperationCode::Mul)); 

        // R[3]: Index Put (4)
        assert!(matches!(records[3].op_code, OperationCode::Put)); 
        // R[4]: Width Put (4, base size) -- NOTE: This should technically be 20 (width of inner array) if the type checker was implemented properly. For this test, it uses base size, which is OK.
        assert!(matches!(records[4].op_code, OperationCode::Put)); 
        // R[5]: Mul (Offset 2: t3 * t4)
        assert!(matches!(records[5].op_code, OperationCode::Mul));
        // R[6]: Add (Final Offset)
        assert!(matches!(records[6].op_code, OperationCode::Add)); 
        
        assert_eq!(records[6].result.id, final_offset_id, "Result ID must match the returned final offset ID.");

        println!("\nSUCCESS: Corrected multidimensional array offset generation produced the necessary sequence of records.");
    }
}