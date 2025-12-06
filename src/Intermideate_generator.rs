

use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

use crate::symbol_table::StaticAddressedEnv;
use crate::syntax_evaluator::Syntaxer;

use super::symbol_table::{Env,Value, AddressedEnv};
use super::syntax_evaluator::{SyntaxTree,SyntaxTreeNode,IndexBox,Opperator};
use super::parser::{TreeControler,Rules};
use super::type_checker::{size_of_value, widen, get_base_type};
use super::lexical_analyzer::{tokens};


#[derive(Debug, Clone)]
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
    Gre,
    Geq,
    Eeq,
    Neq,

    Slef,
    Srig,

    // assignment of form x = op y
    Not,
    Negate,

    // all widens
    WidenToReal,
    WidenToInt,

    // copy instructuon of form x = y
    Put, //
    AsAdderess,

    // unconditional jump of form goto L, where L is a label
    Goto,
    
    // conditional jump of form if x goto L / ifFalse x goto L
    IfTrue,
    IfFalse,
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
    pub fn set_int(i:i32) -> Self {
        Self { contained_value: Value::Int(i) }
    }
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


#[derive(Debug,Clone)]
pub struct CodeMetaData {
    // all values start at 0 lmao
    pub tree_index:usize, 
    pub ast_index:usize, 
    pub current_scope: usize,
    pub loop_exit_pointers: Vec<(usize,usize)>, //format: loop start record, break record locaton
    pub loop_start_location:usize, // tracks record number of loop start, 
    pub result_id: String,
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


#[derive(Debug, Clone)]
pub struct Record {
    source1: AddressValue,
    source2: Option<AddressValue>, // some commands dont need a second source
    op_code: OperationCode,
    result: Option<Address>
}
impl Record {
    pub fn display(&self) -> String {
        if self.result.is_none() {
            // no result meaning code looks like
            if self.source2.is_some() {
                return format!("{:?} {:?} {:?}\n",self.op_code, self.source1.display(), self.source2.clone().unwrap().display());
            } else {
                return format!("{:?} {:?}\n",self.op_code, self.source1.display());
            }
        }
        if self.source2.is_some(){ // 2 source eq
            return format!("{:?} = {:?} {:?} {:?}\n", self.result.clone().unwrap().display(), self.source1.display(), self.op_code, self.source2.clone().unwrap().display());
        } else { // 1 source unary
            return format!("{:?} = {:?} {:?}\n", self.result.clone().unwrap().display(), self.op_code, self.source1.display());
        }
    }
}

pub struct RecordManager {
    pub all_records: Vec<Record>,
    pub tree_controler: TreeControler,
    pub global_env: AddressedEnv,
    pub temp_count: i32,
    pub ast_roots: Vec<SyntaxTree>,
}

impl RecordManager {
    pub fn add_record(&mut self,rec:Record) {
        self.all_records.push(rec);
    }
    pub fn add_record_list(&mut self,rec:&mut Vec<Record>) {
        self.all_records.append(rec);
    }
    pub fn update_record(&mut self,rec_num:usize, new_rec:Record) {
        self.all_records[rec_num] = new_rec;
    }
    pub fn get_record(&self, rec_num:usize) -> &Record {
        &self.all_records[rec_num]
    }
    pub fn print_records(&self) -> String{
        let mut out = "".to_owned();
        let mut rec_num = 0;
        for rec in &self.all_records {
            out = out.to_owned() +&format!("{}: ",rec_num) + &rec.display();
            rec_num +=1 ;
        }
        return out.to_owned();
    }
    fn print_record_vec(vec:&Vec<Record>) -> String {
        let mut out = "".to_owned();
        for rec in vec {
            out = out.to_owned() + &rec.display();
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
    fn generate_assignment_code(&mut self, source:String, sink:String, current_scope:usize) -> Result<Vec<Record>,String>{
        let source_type = self.global_env.find_type_at_scope(source.clone(), current_scope).unwrap();
        let sink_type = self.global_env.find_type_at_scope(sink.clone(), current_scope).unwrap();
        let mut output = Vec::new();

        // type checking and type conversion
        let winden_attempt = widen(&source_type, &sink_type);
        if winden_attempt.is_ok() {
            // check if each one needs widening
            let (wide_source, wide_sink) = winden_attempt.unwrap();
            let mut source_addr = self.get_address(source, current_scope).unwrap();
            let mut sink_addr = self.get_address(sink, current_scope).unwrap();
            if source_type != wide_source {
                let temp_source = self.make_temp(current_scope, wide_source.clone());
                let temp_source_addr = self.get_address(temp_source, current_scope).unwrap();
                output.push(Record {
                    source1: AddressValue::new_addr(source_addr),
                    source2: None,
                    op_code: RecordManager::determine_widen_type(&wide_source).unwrap(),
                    result: Some(temp_source_addr.clone()),
                });
                source_addr = temp_source_addr;
            }
            if sink_type != wide_sink {
                let temp_sink = self.make_temp(current_scope, wide_sink.clone());
                let temp_sink_addr = self.get_address(temp_sink, current_scope).unwrap();
                output.push(Record {
                    source1: AddressValue::new_addr(sink_addr),
                    source2: None,
                    op_code: RecordManager::determine_widen_type(&wide_sink).unwrap(),
                    result: Some(temp_sink_addr.clone()),
                });
                sink_addr = temp_sink_addr;
            }
            output.push(Record {
                source1: AddressValue::new_addr(source_addr),
                source2: None,
                op_code: OperationCode::Put,
                result: Some(sink_addr),
            });
           

            return Ok(output);
        } else {
            return Err(format!("Widen Failure: {}",winden_attempt.unwrap_err()));
        }
    }

    fn determine_widen_type(v:&Value) -> Option<OperationCode>{
        match v {
            Value::AnyFloat | Value::Float(_) => return Some(OperationCode::WidenToReal),
            Value::AnyInt | Value::Int(_) => return Some(OperationCode::WidenToInt),
            _ => return None,
        }
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

    fn repoint_breaks_to_out_of_loop(&mut self, current_loop:usize,exit_index:i32, metadata:CodeMetaData) -> CodeMetaData {
        let mut out_metadata = metadata.clone();
        out_metadata.loop_exit_pointers = vec![]; // clean it
        for (loop_id, record_loc) in metadata.loop_exit_pointers {
            if loop_id == current_loop {
                // break for current loop
                self.update_record(record_loc, Record { 
                    source1: AddressValue::new_const(Constent {contained_value: Value::Int(exit_index as i32)}), 
                    source2: None, 
                    op_code: OperationCode::Goto, 
                    result:  None
                });
            } else {
                // put back into metadata
                out_metadata.loop_exit_pointers.push((loop_id, record_loc));
            }
        }

        return out_metadata;
    }

    // gemini made
    pub fn generate_array_offset(
        &mut self, 
        node_index: IndexBox, 
        arr_id: String, // The base variable name (e.g., "i" for i[2][4])
        current_scope: usize, 
        tree_vec: &mut RefMut<'_, Vec<Rc<RefCell<SyntaxTreeNode>>>>,
        record_list: &mut Vec<Record>
    ) -> Result<(String, Value, Vec<Record>), String> {
        
        let binding = Rc::clone(&tree_vec[node_index.get()]);
        let node = binding.borrow();
        
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
            let (prev_offset_id, prev_type,mut rec_list) = self.generate_array_offset(left_idx, arr_id.clone(), current_scope, tree_vec, record_list)?;
            
            // Generate width for the current dimension based on the type returned by the previous access
            let (current_element_type, element_width) = match &prev_type {
                Value::ArrayOf(inner, _) => (inner.as_ref().clone(), self.calculate_type_size(inner)),
                _ => return Err("Attempted to index a non-array type in recursion".to_string())
            };

            // 2. Generate Code for Index Expression (E) - Index * Width
            let mut right_code = self.generate_ast_code(tree_vec, right_idx.clone(), current_scope)?;
            rec_list.append(&mut right_code.0);
            let index_id = right_code.1;
            let index_addr = self.get_address(index_id.clone(),current_scope)?;

            // Generate Width Constant: w = width
            let width_temp = self.make_temp(current_scope, Value::AnyInt);
            let width_addr = self.get_address(width_temp.clone(),current_scope)?;
            rec_list.push(Record {
                source1: AddressValue::new_const(Constent { contained_value: Value::Int(element_width as i32) }),
                source2: None, op_code: OperationCode::Put, result: Some(width_addr.clone())
            });

            // Generate Multiply: t_mul = index * w
            let mul_temp = self.make_temp(current_scope, Value::AnyInt);
            let mul_addr = self.get_address(mul_temp.clone(),current_scope)?;
            
            rec_list.push(Record {
                source1: AddressValue::new_addr(index_addr),
                source2: Some(AddressValue::new_addr(width_addr)),
                op_code: OperationCode::Mul,
                result: Some(mul_addr.clone())
            });

            // 3. Generate Add: final_offset = prev_offset + t_mul
            let final_offset_temp = self.make_temp(current_scope, Value::AnyInt);
            let final_offset_addr = self.get_address(final_offset_temp.clone(),current_scope)?;
            let prev_offset_addr = self.get_address(prev_offset_id,current_scope)?;

            rec_list.push(Record {
                source1: AddressValue::new_addr(prev_offset_addr),
                source2: Some(AddressValue::new_addr(mul_addr)),
                op_code: OperationCode::Add,
                result: Some(final_offset_addr)
            });
            
            // Return the final offset temp and the type of the element accessed at this level
            return Ok((final_offset_temp, current_element_type, rec_list));

        } else {
            // Base Case: The left child is nothing
            
            // 1. Calculate Width: Get the width of the innermost array element (the base type size).
            let (inner_type, base_element_width) = match &full_array_type {
                Value::ArrayOf(inner, _) => (inner.as_ref().clone(), self.calculate_type_size(inner)),
                _ => return Err(format!("Variable '{}' is not an array", arr_id))
            };

            // 2. Generate Code for Index Expression (E)
            let mut right_code = self.generate_ast_code(tree_vec, right_idx.clone(), current_scope)?;
            record_list.append(&mut right_code.0);
            let index_id = right_code.1;
            let index_addr = self.get_address(index_id.clone(),current_scope)?; // problem get addr

            // Generate Width Constant: w = base_element_width
            let width_temp = self.make_temp(current_scope, Value::AnyInt);
            let width_addr = self.get_address(width_temp.clone(),current_scope)?;
            record_list.push(Record {
                source1: AddressValue::new_const(Constent { contained_value: Value::Int(base_element_width as i32) }),
                source2: None, op_code: OperationCode::Put, result: Some(width_addr.clone())
            });

            // 3. Generate Multiply: offset = index * w
            let offset_temp = self.make_temp(current_scope, Value::AnyInt);
            let offset_addr = self.get_address(offset_temp.clone(),current_scope)?;
            
            record_list.push(Record {
                source1: AddressValue::new_addr(index_addr),
                source2: Some(AddressValue::new_addr(width_addr)),
                op_code: OperationCode::Mul,
                result: Some(offset_addr.clone())
            });

            // put as address
            let address_tmp = self.make_temp(current_scope, inner_type.clone());
            let address_addr = self.get_address(address_tmp.clone(),current_scope)?;

            record_list.push(Record {
                source1: AddressValue::new_addr(offset_addr),
                source2: None,
                op_code: OperationCode::AsAdderess,
                result: Some(address_addr)
            });
            
            // Return the calculated offset and the type of the element
            return Ok((address_tmp, inner_type,record_list.to_vec()));
        };
    }


    // STUB: Generates code for an expression and returns the temp holding the result.
    // Replaces 'generate_code' for the purpose of the array offset function.
    fn generate_expression_stub(
        &mut self, 
        node_idx: IndexBox, 
        scope: usize, 
        tree_vec: &Vec<Rc<RefCell<SyntaxTreeNode>>>
    ) -> Result<(Option<Record>,String), String> {
        let node = tree_vec[node_idx.get()].borrow();
        match &node.opperator {
            Opperator::Terminal(tokens::Number(n)) => {
                let t = self.make_temp(scope, Value::Int(*n));
                let addr = self.get_address(t.clone(),scope)?;
                let record = Record {
                    source1: AddressValue::new_const(Constent{contained_value: Value::Int(*n)}),
                    source2: None, 
                    op_code: OperationCode::Put, 
                    result: Some(addr)
                };
                Ok((Some(record), t))
            },
            Opperator::Terminal(tokens::Id(s)) => {
                 // If it's a variable ID (e.g., 'k' in arr[k]), we just return the ID.
                 Ok((None, s.clone()))
            },
            // Add arithmetic op handling here (Plus, Minus, etc.) calling make_temp recursively
            _ => Err(format!("Expression type {:?} not implemented in stub", node.opperator))
        }
    }

    // end of gemini

    // recursive code generation
    /// output schema. data or error string
    pub fn generate_code(&mut self, data:CodeMetaData) -> Result<CodeMetaData,String>{
        let mut_controler = self.tree_controler.clone();
        let top_node = mut_controler.get_weak(data.tree_index).unwrap();
        // 1. deal with the block at hand before dealing with children

        let current_rule = &top_node.rule;
        // make local mutable copies.
        let mut local_data = data.clone();
        let current_scope = data.current_scope;
        let ast_index = data.ast_index;
        //println!("{:?}",local_data); //********************************************** debug line stick
        match current_rule.clone() {
            // increase the scope #
            Rules::ProgramToBlock | Rules::StmtToBlock=>  {
                local_data.current_scope +=1;
            }
            
            // nothings
            Rules::BlockToEpsilon | Rules::StmtToDecl | Rules::StmtToExpr | Rules::ElifToElse | Rules::ElifToEpsilon | Rules::Terminal(_) | Rules::BlockToStmtBlock => {
                // nothing ever happens 
            }

            // "specal" cases 
            Rules::StmtToBreak => {
                // break intresting case bc we need to "break" out of current loop

                // goto loop exit
                let break_loc = self.all_records.len();
                self.add_record(Record { 
                    source1: AddressValue::new_const(Constent {contained_value: Value::AnyInt}), 
                    source2: None, 
                    op_code: OperationCode::Goto, 
                    result:  None
                });
                
                // Add to returned metadata
                local_data.loop_exit_pointers.push((local_data.loop_start_location,break_loc));
                return Ok(local_data);
            },
            Rules::StmtToWhile => {
                // complex one where i have to structure it like the following
                // 1: condition code -> res
                // 2: iffalse res 5 (if false jumps to 5)
                // 3: stmt code
                // 4: goto 1
                // 5. next block


                // 1 condition
                let cond_start = self.all_records.len();
                // 1.1 redefine start of loop to cond start
                local_data.loop_start_location = cond_start;
                local_data.tree_index = top_node.children[2]; // third child is condition start
                //generate cond data
                let mut post_cond_data = self.generate_code(local_data.clone())?;
                let res_id = post_cond_data.result_id.clone();
                
                
                

                // 2 iffalse
                let false_record_ind = self.all_records.len().clone();
                self.add_record(Record { 
                    source1: AddressValue::new_addr(self.get_address(res_id.clone(), post_cond_data.current_scope)?),
                    source2: Some(AddressValue::new_const(Constent {contained_value: Value::AnyInt})), // this will be changed later. thats why we have the index saved
                    op_code: OperationCode::IfFalse, 
                    result:  None
                });
                //println!("***********While: false rec ind {} and gathered record {}",false_record_ind, self.get_record(false_record_ind).display());
                

                // 3 stmt code
                post_cond_data.tree_index =  top_node.children[4];  // fith child is stmt start
                let post_stmt_data = self.generate_code(post_cond_data.clone())?;
                

                // 4 goto 1
                self.add_record(Record { 
                    source1: AddressValue::new_const(Constent::set_int(cond_start as i32)),
                    source2: None,
                    op_code: OperationCode::Goto, 
                    result:  None
                });
                
                // 5, next block (dont do anything here but make the record number here where the og pointer goes to)
                let next_block_id = self.all_records.len();
                self.update_record(false_record_ind, Record { 
                    source1: AddressValue::new_addr(self.get_address(res_id, local_data.current_scope)?),
                    source2: Some(AddressValue::new_const(Constent {contained_value: Value::Int(next_block_id as i32)})),
                    op_code: OperationCode::IfFalse, 
                    result:  None
                });

                // 5.5 update all other breaks
                let post_break_fix_data = self.repoint_breaks_to_out_of_loop(cond_start, next_block_id as i32, post_stmt_data);
                // dont update tree data, 
                return Ok(post_break_fix_data);
            },
            Rules::StmtToDo => {
                // similer to while but in a diffrent easier order :sob:
                // structure
                // 1: stmt to do
                // 2: condition
                // 3: iftrue 1
                // 4. next block (just return and continue lmao)

                // 1 stmt code
                let top_of_do_index = self.all_records.len();
                local_data.tree_index =  top_node.children[1];
                let mut post_stmt_data = self.generate_code(local_data.clone())?;
                
                // 2 condition code
                post_stmt_data.loop_start_location = top_of_do_index;
                post_stmt_data.tree_index = top_node.children[4];
                //generate cond data
                let post_cond_data = self.generate_code(post_stmt_data)?;
                let res_id = post_cond_data.result_id.clone();
                let res_addr = self.get_address(res_id.clone(), post_cond_data.current_scope.clone())?;

                // 3, iftrue
                self.add_record(Record { 
                    source1: AddressValue::new_addr(res_addr),
                    source2: Some(AddressValue::new_const(Constent {contained_value: Value::Int(top_of_do_index as i32)})),
                    op_code: OperationCode::IfTrue, 
                    result:  None
                });

                
                // deal with any breaks
                let post_break_data = self.repoint_breaks_to_out_of_loop(top_of_do_index, self.all_records.len() as i32, post_cond_data);
                return Ok(post_break_data);
            },
            Rules::StmtToIf => {
                // structure if ( condition ) stmt elif
                // 1 condition
                // 2 iftrue 4
                // 3 iffalse 6
                // 4 stmt
                // 5 go to end of 6
                // 6 elif

                // 1 condition
                let cond_start = self.all_records.len();
                // 1.1 redefine start of loop to cond start
                local_data.loop_start_location = cond_start;
                local_data.tree_index = top_node.children[2]; // third child is condition start
                //generate cond data
                let mut post_cond_data = self.generate_code(local_data.clone())?;
                let res_id = post_cond_data.result_id.clone();

                // dummy if true and if false
                // 2, iftrue, points to 4, its a calculatable number of records away
                self.add_record(Record { 
                    source1: AddressValue::new_addr(self.get_address(res_id.clone(), local_data.current_scope)?),
                    source2: Some(AddressValue::new_const(Constent {contained_value: Value::Int(self.all_records.len() as i32 +2)})), // jump over self and next one to start at 4
                    op_code: OperationCode::IfTrue, 
                    result:  None
                });

                // 3, iffalse
                let iffalse_record_ind = self.all_records.len();
                self.add_record(Record { 
                    source1: AddressValue::new_addr(self.get_address(res_id.clone(), local_data.current_scope)?),
                    source2: Some(AddressValue::new_const(Constent {contained_value: Value::AnyInt})),
                    op_code: OperationCode::IfFalse, 
                    result:  None
                });

                // 4 stmt
                post_cond_data.tree_index =  top_node.children[4];
                let mut post_stmt_data = self.generate_code(post_cond_data.clone())?;

                // 5 go to elif end
                let stmt_break_position = self.all_records.len();
                self.add_record(Record { 
                    source1: AddressValue::new_const(Constent {contained_value: Value::AnyInt}),
                    source2: None,
                    op_code: OperationCode::Goto, 
                    result:  None
                });

                // 6 elif
                post_stmt_data.tree_index =  top_node.children[5];
                let post_elif_data = self.generate_code(post_stmt_data.clone())?;

                // deal with after elif
                let post_elif_pos = self.all_records.len() as i32;
                self.update_record(stmt_break_position, Record { 
                    source1: AddressValue::new_const(Constent {contained_value: Value::Int(post_elif_pos)}),
                    source2: None,
                    op_code: OperationCode::Goto, 
                    result:  None
                });
                self.update_record(iffalse_record_ind, Record { 
                    source1: AddressValue::new_addr(self.get_address(res_id.clone(), local_data.current_scope)?),
                    source2: Some(AddressValue::new_const(Constent {contained_value: Value::Int(post_elif_pos)})),
                    op_code: OperationCode::IfFalse, 
                    result:  None
                });
                // deal with any breaks
                let post_break_data = self.repoint_breaks_to_out_of_loop(cond_start, self.all_records.len() as i32, post_elif_data);
                return Ok(post_break_data);
                
            },

            // variable init hell
            // Ruling, not my problem, vars are already in scope from pervious step
            Rules::DeclToVar => {
                return Ok(local_data);
            },
            
            // activate ast and forget about it
            x if Syntaxer::detect_ast_start_node(&x) => {
                // there is an ast about this
                let this_ast = self.ast_roots[ast_index].clone();
                let tree_vec_clone = Rc::clone(&this_ast.tree_vector);
                let mut tree_vec = tree_vec_clone.borrow_mut();
                let top_ind = this_ast.top_node;
                
                // launch ast
                //println!("Launched ast tree num {}:",ast_index );
                //tree_vec[top_ind.get()].borrow().render_node(&tree_vec, 0,true);
                let mut ast_output = self.generate_ast_code(&mut tree_vec, top_ind, current_scope)?;
                //println!("Ended with code:\n{}And result {}\n", Self::print_record_vec(&ast_output.0),  ast_output.1);
                
                self.add_record_list(&mut ast_output.0);

                local_data.ast_index +=1;
                local_data.result_id = ast_output.1;
                return Ok(local_data);
            }


            // parser only rules
            Rules::Jump(_) | Rules::ExitEnviornment | Rules::EnterEnviornment  | Rules::ElifMarker | Rules::StmtMarker => {return Err(format!("Encountered parser only rule: {:?}", current_rule));},
            x => {
                mut_controler.render_node(top_node.parent.unwrap(), 0);
                return Err(format!("Encountered Invalid rule: {:?}", x));
            }
        }
        
        // now take care of children

        // expected types: stmts or blocks into somethin. or terminal into nothin idk stuff where children are generalliy independent.
        let current_scope = local_data.current_scope;
        for child in top_node.children.clone(){
            // like run the children i guess. always update the metadata ovc
            local_data.tree_index = child;
            local_data = self.generate_code(local_data)?;
            local_data.current_scope = current_scope; // always reset current scope?
        }

        return Ok(local_data);
    }

    // recursive function that generates ast code.
    /// output schema, list of all records, id of (temp) var where result is stored or error string
    pub fn generate_ast_code(&mut self, tree_vec: &mut RefMut<'_, Vec<Rc<RefCell<SyntaxTreeNode>>>>,current_ind:IndexBox, current_scope: usize) -> Result<(Vec<Record>, String),String>{
        // step 1 recurs into leaves
        let binding = Rc::clone(&tree_vec[current_ind.get()]);
        let this_node = &binding.borrow();
        let children = &this_node.child_nodes;
        let mut right_child_code:Option<(Vec<Record>, String)> = None;
        let mut left_child_code:Option<(Vec<Record>, String)> = None;
        if children.0.is_some() && (this_node.opperator != Opperator::ArrayAccess){left_child_code = Some(self.generate_ast_code(tree_vec, children.0.clone().unwrap(), current_scope)?);}
        if children.1.is_some() && (this_node.opperator != Opperator::ArrayAccess){right_child_code = Some(self.generate_ast_code(tree_vec, children.1.clone().unwrap(), current_scope)?);}

        // deal with current operation with 2 things in mind.
            // the result of lower 2 are used as inputs
            // the result of current opperation is input for upper opperation.
        match this_node.opperator.clone() {
            Opperator::Terminal(token) => {
                // no expected children so we dont need to deal with them
                // load terminal into temp var
                // these are of 4 types. int real true false
                match token {
                    tokens::Number(n) => {
                        let tmp = self.make_temp(current_scope, Value::Int(n));
                        let record = Record {
                            source1: AddressValue::new_const(Constent { contained_value: Value::Int(n) }),
                            source2: None,
                            op_code: OperationCode::Put,
                            result: Some(self.get_address(tmp.clone(),current_scope)?)
                        };
                        return Ok((vec![record], tmp));
                    }
                    tokens::Real(r) => {
                        let tmp = self.make_temp(current_scope, Value::Float(r));
                        let record = Record {
                            source1: AddressValue::new_const(Constent { contained_value: Value::Float(r) }),
                            source2: None,
                            op_code: OperationCode::Put,
                            result: Some(self.get_address(tmp.clone(),current_scope)?)
                        };
                        return Ok((vec![record], tmp));
                    }
                    tokens::True => {
                        let tmp = self.make_temp(current_scope, Value::AnyBool);
                        let record = Record {
                            source1: AddressValue::new_const(Constent { contained_value: Value::Bool(true) }),
                            source2: None,
                            op_code: OperationCode::Put,
                            result: Some(self.get_address(tmp.clone(),current_scope)?)
                        };
                        return Ok((vec![record], tmp));}
                    tokens::False => {
                        let tmp = self.make_temp(current_scope, Value::AnyBool);
                        let record = Record {
                            source1: AddressValue::new_const(Constent { contained_value: Value::Bool(false) }),
                            source2: None,
                            op_code: OperationCode::Put,
                            result: Some(self.get_address(tmp.clone(),current_scope)?)
                        };
                        return Ok((vec![record], tmp));
                    }
                    tokens::Id(id) => {
                        // if this has type array do something
                        let id_typ = self.get_address(id.clone(), current_scope)?.typ;
                        if let Value::ArrayOf(_, _) =  id_typ.clone(){
                            // return right child code as thats where array access starts
                            if right_child_code.is_some(){
                                return Ok(right_child_code.unwrap());
                            }
                            return Err(format!("Array ID falty right side. id: {}, type: {}", id, id_typ.render()));
                        } 
                        return Ok((vec![], id.clone()));
                    }
                    _  => { return Err(format!("Invalid Terminal {:?}",token));}
                }
            }
            Opperator::Enclose => {
                // mostly serves as a deliniation so rotations dont happen where they arnt supposed to. pass everything through. 
                // only should have one argument on the left
                let left_result = left_child_code.unwrap();
                return Ok(left_result);
            }
            Opperator::ArrayAccess => {
                let binding = Rc::clone(&tree_vec[this_node.parent.clone().unwrap().get()]);
                let parent = &binding.borrow();
                let parent_id = parent.opperator.unwrap_terminal().unwrap().extract_id_value().unwrap();
                let res= self.generate_array_offset(current_ind, parent_id, current_scope, tree_vec, &mut vec![]);
                let (offset_id, _final_size, code) = res.unwrap();
                return Ok((code, offset_id));
            }
            Opperator::Negate => { // math negate, its diffrent
                // thing to negate on the left

                //1. get thing to negate
                let mut result = left_child_code.unwrap();
                // 2 make one line of code that negates whatever the thing is
                let typ = self.get_address(result.1.clone(), current_scope).unwrap().typ;
                let tmp = self.make_temp(current_scope, typ.clone()); // can be int or real
                let record = Record {
                    source1: AddressValue::new_addr(self.get_address(result.1, current_scope)?),
                    source2: None,
                    op_code: OperationCode::Negate,
                    result: Some(self.get_address(tmp.clone(),current_scope)?)
                };
                result.0.push(record);
                return Ok((result.0, tmp));
            }
            Opperator::Not => {
                //1. get thing to negate
                let mut result = left_child_code.unwrap();
                // 2 make one line of code that negates whatever the thing is
                let tmp = self.make_temp(current_scope, Value::AnyBool);
                let record = Record {
                    source1: AddressValue::new_addr(self.get_address(result.1, current_scope)?),
                    source2: None,
                    op_code: OperationCode::Not,
                    result: Some(self.get_address(tmp.clone(),current_scope)?)
                };
                result.0.push(record);
                return Ok((result.0, tmp));
            }
            Opperator::Assigns => {
                // 2 things here.
                // right: calculated result
                // left: where to put it
                // get source
                let mut source = right_child_code.unwrap();
                // get sink
                let mut sink = left_child_code.unwrap();

                // check source or sink for being arraysl(_);
                // make record linking them
                let record = self.generate_assignment_code(source.1,sink.1.clone(), current_scope);

                if record.is_err() {
                    return Err(format!("Assignment err: {}", record.unwrap_err()));
                }

                source.0.append(&mut sink.0);
                source.0.append(&mut record.unwrap());
                return Ok((source.0, sink.1));
            }
            x if  matches!(x, Opperator::Plus | Opperator::Minus | Opperator::Multiply | Opperator::Divide | Opperator::Equals | Opperator::NotEquals | Opperator::Less | Opperator::LessEquals | Opperator::Greater | Opperator::GreaterEquals | Opperator::And | Opperator::Or | Opperator::ShiftLeft | Opperator::ShiftRight) => {
                // add left and right. these must follow widen rules :(
                // also this is a general opperator where its the same process for all other eq types.
                
                

                // 0. get both children & types
                let right = right_child_code.unwrap();
                let right_type = self.get_address(right.1.clone(), current_scope).unwrap().typ;
                // get sink
                let mut left = left_child_code.unwrap();
                let left_type = self.get_address(right.1.clone(), current_scope).unwrap().typ;

                // 0.5 dump both recod lists into one
                let mut rec_list = right.0;
                rec_list.append(&mut left.0);

                // 1. check widen
                let widen_res = widen(&right_type, &left_type);
                if widen_res.is_err() {return Err(format!("Math Error: {}",widen_res.unwrap_err()));}
                let widen_res = widen_res.unwrap();

                // 2. if wider, add code that fixes that
                let right_tmp:String;
                let left_tmp:String;
                if widen_res.0 != right_type {
                    right_tmp = self.make_temp(current_scope, widen_res.0.clone());
                    rec_list.push(Record { 
                        source1: AddressValue::new_addr(self.get_address(right.1, current_scope)?) , 
                        source2: None, 
                        op_code: Self::determine_widen_type(&widen_res.0).unwrap(), 
                        result: Some(self.get_address(right_tmp.clone(), current_scope)?) 
                    });
                }else {
                    right_tmp = right.1;
                }
                if widen_res.1 != left_type {
                    left_tmp = self.make_temp(current_scope, widen_res.1.clone());
                    rec_list.push(Record { 
                        source1: AddressValue::new_addr(self.get_address(left.1, current_scope)?) , 
                        source2: None, 
                        op_code: Self::determine_widen_type(&widen_res.1).unwrap(), 
                        result: Some(self.get_address(left_tmp.clone(), current_scope)?) 
                    });
                } else {
                    left_tmp = left.1;
                }

                
                // 3. now we have widened addresses do the opperation.
                let op: OperationCode; 
                let res_type: Value;
                match x {
                    Opperator::Plus => {
                        res_type = widen_res.0.generalize();
                        op =  OperationCode::Add;
                    },
                    Opperator::Minus => {
                        res_type = widen_res.0.generalize();
                        op =  OperationCode::Sub;
                    },
                    Opperator::Multiply => {
                        res_type = widen_res.0.generalize();
                        op =  OperationCode::Mul;
                    },
                    Opperator::Divide => {
                        res_type = widen_res.0.generalize();
                        op =  OperationCode::Dev;
                    },
                    Opperator::ShiftLeft => {
                        res_type = widen_res.0.generalize();
                        op =  OperationCode::Slef;
                    },
                    Opperator::ShiftRight => {
                        res_type = widen_res.0.generalize();
                        op =  OperationCode::Srig;
                    },
                    Opperator::Equals => {
                        res_type = Value::AnyBool;
                        op =  OperationCode::Eeq;
                    },
                    Opperator::NotEquals => {
                        res_type = Value::AnyBool;
                        op =  OperationCode::Neq;
                    },
                    Opperator::Less => {
                        res_type = Value::AnyBool;
                        op =  OperationCode::Les;
                    },
                    Opperator::LessEquals => {
                        res_type = Value::AnyBool;
                        op =  OperationCode::Leq;
                    },
                    Opperator::Greater => {
                        res_type = Value::AnyBool;
                        op =  OperationCode::Gre;
                    },
                    Opperator::GreaterEquals => {
                        res_type = Value::AnyBool;
                        op =  OperationCode::Geq;
                    },
                    Opperator::And => {
                        res_type = Value::AnyBool;
                        op =  OperationCode::And;
                    },
                    Opperator::Or => {
                        res_type = Value::AnyBool;
                        op =  OperationCode::Or;
                    },
                    _ => {return Err("Invalid Opperator".to_owned());}
                }


                let result_tmp = self.make_temp(current_scope, res_type); // it dont matter whitch one we use as long as we generilize it
                rec_list.push(Record { 
                    source1: AddressValue::new_addr(self.get_address(left_tmp, current_scope)?), 
                    source2: Some(AddressValue::new_addr(self.get_address(right_tmp, current_scope)?)), 
                    op_code: op, 
                    result: Some(self.get_address(result_tmp.clone(), current_scope)?) 
                });

                return Ok((rec_list,result_tmp));
            }
            _ => {
                return Err(format!("No matching opperator"));
            }
        }
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
        result: Some(addr_t3.clone()),
    });
    
    // t1 = Put t3
    rec_manager.add_record(Record {
        source1: AddressValue::new_addr(addr_t3.clone()),
        source2: None,
        op_code: OperationCode::Put,
        result: Some(addr_t1.clone()),
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
        result: Some(addr_t1),
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
        result: Some(addr_v1.clone()),
    });

    // Record 2: res_r = Put v1
    rec_manager.add_record(Record {
        source1: AddressValue::new_addr(addr_v1.clone()),
        source2: None,
        op_code: OperationCode::Put,
        result: Some(addr_res.clone()),
    });
    
    // Record 3: v2 = res_r - v1
    rec_manager.add_record(Record {
        source1: AddressValue::new_addr(addr_res.clone()),
        source2: Some(AddressValue::new_addr(addr_v1.clone())),
        op_code: OperationCode::Sub,
        result: Some(addr_v2.clone()),
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
        result: Some(addr_t2.clone()),
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
        let mut tree_vec = Rc::new(RefCell::new(vec![
            idx_4_node,          // 0: Index E2 (4)
            idx_2_node,          // 1: Index E1 (2)
            arr_id_node,         // 2: Base ID 'i' (Not used directly by function, but used by inner access)
            inner_access_node,   // 3: Inner Access i[2]
            sub,   // 4: Root i[2][4]
        ]));
        
        final_access_node.borrow().render_node(&tree_vec.borrow(), 0, true);
        // 4. GENERATE CODE: Pass the ROOT node index and the BASE variable ID
        let result = rec_manager.generate_array_offset(IndexBox::new(4), "i".to_string(), 0, &mut tree_vec.borrow_mut(), &mut vec![]);
        
        assert!(result.is_ok(), "Code generation failed: {}", result.unwrap_err());
        let (final_offset_id, final_type, mut rec_list) = result.unwrap();

        // 5. ASSERTIONS

        // A. Final Type Check: Should resolve to the base element type (AnyInt)
        assert_eq!(final_type, Value::AnyFloat, "Final type should be the base element type (AnyFloat).");

        // B. Instruction Sequence Check\
        rec_manager.add_record_list(&mut rec_list);
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

        assert_eq!(records.len(), 8, "Expected exactly 7 records for the expression i[2][4].");
        
        // Check types and order: (2 Puts for Indices) + (Put/Mul for first index) + (Put/Mul/Add for second index)
        // R[0]: Index Put (2)
        assert!(matches!(records[0].op_code, OperationCode::Put)); 
        assert!(matches!(records[1].op_code, OperationCode::Put)); 
        assert!(matches!(records[2].op_code, OperationCode::Mul)); 
        assert!(matches!(records[3].op_code, OperationCode::AsAdderess)); 
        assert!(matches!(records[4].op_code, OperationCode::Put)); 
        assert!(matches!(records[5].op_code, OperationCode::Put)); 
        assert!(matches!(records[6].op_code, OperationCode::Mul));
        assert!(matches!(records[7].op_code, OperationCode::Add)); 
        
        assert_eq!(records[7].result.clone().unwrap().id, final_offset_id, "Result ID must match the returned final offset ID.");

        println!("\nSUCCESS: Corrected multidimensional array offset generation produced the necessary sequence of records.");
    }
}