use std::{mem::size_of, ops::Deref, rc::Rc};

use super::symbol_table::{Env,Value};
//use super::parser::{TreeNode,TreeControler};

#[derive(Debug)]
pub enum MemoryType {
    Scaler,
    Array
}



pub fn size_calculator(env:&Rc<Env>) -> Result<Vec<(MemoryType,String,Value, Vec<usize>,usize)>, String> {
    let working_env = env.clone();
    let mut working_table: Vec<(MemoryType,String,Value, Vec<usize>,usize)> = vec![];
    
    // compute all internal data types
    for (id, value) in working_env.table.borrow().iter() {
        let true_value = get_base_type(Rc::new(value.clone()), vec![]);
        if true_value.1.len() != 0 {
            let res = size_of_value(&true_value.0);
            if res.is_err(){
                return Err(res.unwrap_err());
            }
            let mut size = res.unwrap();
            for s in &true_value.1 {
                size *= *s as usize;
            }

            working_table.push((MemoryType::Array,id.to_string(),true_value.0,true_value.1,size));
        } else {
            let res = size_of_value(&true_value.0);
            if res.is_err(){
                return Err(res.unwrap_err());
            }
            let size = res.unwrap();

            working_table.push((MemoryType::Scaler,id.to_string(),true_value.0,true_value.1,size));
        }
    }

    for weak_child in working_env.children.borrow().iter() {
        if let Some(child_rc) = weak_child.upgrade() {
            let child_table = size_calculator(&child_rc); 
            if child_table.is_ok() {
                working_table.append(&mut child_table.unwrap());
            }else {
                return Err(format!("Child Failed"));
            }
        } else {
            return Err(format!("[Child scope was dropped]"));
        }
    }
 

    return Ok(working_table);
}

pub fn size_of_value(v:&Value) -> Result<usize,String> {
    match v {
        Value::AnyInt | Value::Int(_) | Value::Null=> {
            return Ok(size_of::<i32>());
        },
        Value::AnyBool | Value::Bool(_) => {
            return Ok(size_of::<bool>());
        }
        Value::AnyFloat | Value::Float(_) => {
            return Ok(size_of::<f64>());
        }
        Value::ArrayOf(_, _) => {
            return Ok(array_size(&v.clone()));
        }
        _v => {
            return Err(format!("Failed to parse True value of: {:?}", _v));
        }
    }
}
fn array_size(v: &Value) -> usize {
    match v {
        Value::ArrayOf(inner, len) => {
            array_size(inner) * (*len as usize)
        },
        _ => {
            size_of_value(v).unwrap_or(4) 
        }
    }
}

pub fn get_base_type(v:Rc<Value>,mut arr:Vec<usize>) -> (Value,Vec<usize>) {
    match v.deref() {
        Value::AnyInt => return (Value::AnyInt, arr),
        Value::Int(_) => return (Value::AnyInt, arr),
        Value::AnyBool => return (Value::AnyBool, arr),
        Value::Bool(_) => return (Value::AnyBool, arr),
        Value::AnyFloat => return (Value::AnyFloat, arr),
        Value::Float(_) => return (Value::AnyFloat, arr),
        Value::Null => return (Value::Null, arr),
        Value::ArrayOf(value, s) => {arr.push(*s as usize); return get_base_type(value.clone(), arr)},
    }
}

pub fn widen(first:Value, second:Value) -> Result<(Value,Value), String>{
    // this language only has like 3 types this is a very sumple function
    // float > int
    match first {
        Value::AnyFloat | Value::Float(_)  => {
            if !matches!(second, Value::ArrayOf(_,_ ) | Value::Null | Value::AnyBool | Value::Bool(_)){
                if let Value::Float(_) = second {
                    return Ok((first, second));
                }
                if let Value::Int(i) = second {
                    return Ok((first, Value::Float(i as f64)));
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
                    return Ok((Value::Float(i as f64), second));
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