//parser.rs

use std::collections::{VecDeque};

use super::lexical_analyzer::{tokens};
use super::symbol_table::{Env, Value};




#[derive(Debug)]
pub struct TreeNode {
    parent: Option<usize>,
    children: Vec<usize>,
    rule:Rules,
    valuetype: Value
}

impl TreeNode {
    fn program() -> Self{
        TreeNode{
            parent: None,
            children: vec![],
            rule: Rules::ProgramToBlock,
            valuetype: Value::Null,
        }
    }
    fn child(parent_index:usize, rule:Rules,value:Value) -> Self{
        TreeNode{
            parent: Some(parent_index),
            children: vec![],
            rule: rule,
            valuetype: value,
        }
    }
}

#[derive(Debug)]
pub struct TreeControler {
    tree_vector: Vec<TreeNode>
}

impl TreeControler {
    pub fn new() -> Self{
        TreeControler{
            tree_vector: vec![TreeNode::program()]
        }
    }
    
    pub fn get(&mut self, node_index:usize) -> Option<&mut TreeNode>{
        return self.tree_vector.get_mut(node_index);
    }

    pub fn put(&mut self, parent_index: usize, child:TreeNode) -> usize{
        let child_position = self.tree_vector.len();
        let parent = self.get(parent_index);

        if parent.is_some(){
            let mut_parent = parent.unwrap();
            mut_parent.children.push(child_position);
            self.tree_vector.push(child);
        }        
        return child_position;
    }
}

#[derive(Debug,Clone,PartialEq,)]
enum Rules{
    // Parser commands
    Jump(usize), // jumps counter n spaces

    // program rules
    ProgramToBlock,

    // block rules
    BlockToStmtBlock,
    BlockToEpsilon,

    // stmt rules
    StmtMarker,
    StmtToBlock,
    StmtToDecl,
    StmtToExpr,
    StmtToBreak,
    StmtToWhile,
    StmtToDo,
    StmtToIf,
    
    // decl rules
    DeclToVar,

    // var rules
    VarToID,
    VarToIDArray,
    
    // expr rules
    ExprToAssignEquation,

    // equation rules
    EquationToAddition,
    EquationToSubtraction,
    EquationToMultiplication,
    EquationToDevision,
    EquationToEncloseHeq,
    EquationToObject,

    //heq rules
    HeqMarker,
    HeqToAdd,
    HeqToSubtract,
    HeqToMultiply,
    HeqToDevide,
    HeqToEpsilon,

    // object rules
    ObjectToVar,
    ObjectToNum,
    ObjectToReal,
    ObjectToTrue,
    ObjectToFalse,

    // condition rules
    ConditionMarker,
    ConditionToEquals,
    ConditionToNotEquals,
    ConditionToLess,
    ConditionToLessEq,
    ConditionToMore,
    ConditionToMoreEq,
    ConditionToAnd,
    ConditionToOr,
    ConditionToEnclosed,
    ConditionToObject,


    // hcon rules
    HconMarker,
    HconToEquals,
    HconToLess,
    HconToLessEq,
    HconToMore,
    HconToMoreEq,
    HconToAnd,
    HconToOr,
    HconToNoteq,
    HconToEpsilon,

    // elif
    ElifMarker,
    ElifToElse,
    ElifToEpsilon,

    // terminal
    Terminal(tokens)

}

impl Rules {
    pub fn render(&self)-> String{
        match self {
            Rules::ProgramToBlock => { return "program -> { block }".to_string()},

            Rules::BlockToStmtBlock => { return "block -> stmt block".to_string()},
            Rules::BlockToEpsilon => { return "block -> ε".to_string()},

            Rules::StmtToBlock => { return "stmt -> { block }".to_string()},
            Rules::StmtToDecl => { return "stmt -> decl".to_string()},
            Rules::StmtToExpr => { return "stmt -> expr".to_string()},
            Rules::StmtToBreak => { return "stmt -> break;".to_string()},
            Rules::StmtToWhile => { return "stmt -> while ( condition ) { block }".to_string()},
            Rules::StmtToDo => { return "stmt -> do stmt while ( condition )".to_string()},
            Rules::StmtToIf => { return "stmt -> if ( condition ) stmt elif".to_string()},

            Rules::DeclToVar => { return "decl -> basic var ;".to_string()},

            Rules::VarToID => { return "var -> id".to_string()},
            Rules::VarToIDArray => { return "var -> id [ object ]".to_string()},

            Rules::ExprToAssignEquation => { return "expr -> var = equation ;".to_string()},

            Rules::EquationToAddition => { return "equation -> object + equation".to_string()},
            Rules::EquationToSubtraction => { return "equation -> object - equation".to_string()},
            Rules::EquationToMultiplication => { return "equation -> object * equation".to_string()},
            Rules::EquationToDevision => { return "equation -> object / equation".to_string()},
            Rules::EquationToEncloseHeq => { return "equation -> ( equation )".to_string()},
            Rules::EquationToObject => { return "equation -> object".to_string()},

            Rules::ObjectToVar => { return "object -> var".to_string()},
            Rules::ObjectToNum => { return "object -> num".to_string()},
            Rules::ObjectToReal => { return "object -> real".to_string()},
            Rules::ObjectToTrue => { return "object -> true".to_string()},
            Rules::ObjectToFalse => { return "object -> false".to_string()},

            Rules::ConditionToEquals => { return "condition -> object == condition".to_string()},
            Rules::ConditionToLess => { return "condition -> object < condition".to_string()},
            Rules::ConditionToLessEq => { return "condition -> object <= condition".to_string()},
            Rules::ConditionToMore => { return "condition -> object > condition".to_string()},
            Rules::ConditionToMoreEq => { return "condition -> object >= condition".to_string()},
            Rules::ConditionToAnd => { return "condition -> object && condition".to_string()},
            Rules::ConditionToOr => { return "condition -> object || condition".to_string()},
            Rules::ConditionToEnclosed => { return "condition -> ( condition )".to_string()},
            Rules::ConditionToObject => { return "condition -> object".to_string()},

            Rules::ElifToElse => { return "elif -> else stmt".to_string()},
            Rules::ElifToEpsilon => { return "elif -> ε".to_string()},
            Rules::Terminal(_) => { return "".to_string()},

            _=> {return "Not Implemented".to_string();}
        }
    }
}



pub struct Parser{
    
}

impl Parser {
    pub fn parse(input: Vec<tokens>, table:Env) -> Result<TreeControler, String>{
        let mut current_input_index:usize = 1;
        let mut tree = TreeControler::new();
        let mut current_rule = Rules::ProgramToBlock;
        let mut current_rule_parent:usize = 0;
        let mut rules_to_process: VecDeque<(Rules, usize)> = VecDeque::new(); // acting as a heap


        // inject first start enclose into the system
        if input[0] == tokens::StartEnclose {
            tree.put(0, TreeNode::child(0, Rules::Terminal(tokens::StartEnclose), Value::Null));
        } else {
            return Err("Program didnt start with a { wrap all code in {}'s".to_owned());
        }


        while current_input_index <= input.len(){
            match current_rule  {

                // deal with blocks
                Rules::ProgramToBlock | Rules::StmtToBlock => {
                    //rule only encountered in 2 sinarios. Start of program. or block that looks like this { block }
                    // either way it makes a block that looks like thiis { block } so only deal with that and error otherwise
                    
                    
                    //make all blocks
                    let start_enclose = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::StartEnclose), Value::Null);
                    let stmt_block = TreeNode::child(current_rule_parent, Rules::BlockToStmtBlock, Value::Null); 
                    let end_enclose = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::EndEnclose), Value::Null);

                    //inject into parent
                    tree.put(current_rule_parent, start_enclose);
                    let stmt_index = tree.put(current_rule_parent, stmt_block);
                    tree.put(current_rule_parent, end_enclose);

                    // push next rule
                    rules_to_process.push_front((Rules::BlockToStmtBlock, stmt_index));
                    current_input_index += 1; // advance 1 token
                    break;
                },

                // specal combination. Statement block deals withe verything block
                Rules::BlockToStmtBlock | Rules::StmtMarker => 'outer: {
                    // all this rule is saying is that we are allowing 2 statements in a row.
                    // logic: use this to deal with a statement (something ending with ';')
                    // logic: detect } symbol to end block and return epsilon


                    // recurse. placed first so its exicuted after whatever is inside the statement.
                    if current_rule == Rules::BlockToStmtBlock{
                        rules_to_process.push_front((Rules::BlockToStmtBlock, current_rule_parent));
                    }

                    match &input[current_input_index] {
                        tokens::EndEnclose => {
                            // remove the recursion
                            rules_to_process.pop_front();

                            // epsilon
                            rules_to_process.push_front((Rules::BlockToEpsilon, current_rule_parent));
                            break 'outer; // break to outer match
                        }

                        // detect all types of statements
                        tokens::StartEnclose => 'inner: {
                            // { block }
                            let stmt_block = TreeNode::child(current_rule_parent, Rules::StmtToBlock, Value::Null);
                            let stmt_index = tree.put(current_rule_parent, stmt_block);
                            rules_to_process.push_front((Rules::BlockToStmtBlock, stmt_index));
                            current_input_index += 1; // advance 1 token
                            break 'inner;
                        }

                        tokens::Basic(v) => 'inner: {
                            // decl
                            let decl_block = TreeNode::child(current_rule_parent, Rules::StmtToDecl, v.clone());
                            let decl_index = tree.put(current_rule_parent, decl_block);
                            
                            rules_to_process.push_front((Rules::StmtToDecl, decl_index));
                            break 'inner;
                        }

                        tokens::Break => 'inner: {
                            // break ;
                            let break_block = TreeNode::child(current_rule_parent, Rules::StmtToBreak,  Value::Null);
                            let break_index = tree.put(current_rule_parent, break_block);
                            
                            rules_to_process.push_front((Rules::StmtToBreak, break_index));
                            break 'inner;
                        }

                        tokens::While => 'inner: {
                            // while ( condition ) { block }
                            let block = TreeNode::child(current_rule_parent, Rules::StmtToWhile,  Value::Null);
                            let index = tree.put(current_rule_parent, block);
                            
                            rules_to_process.push_front((Rules::StmtToWhile, index));
                            break 'inner;
                        }

                        tokens::Do => 'inner: {
                            // do stmt while ( condition ) ;
                            let block = TreeNode::child(current_rule_parent, Rules::StmtToDo, Value::Null);
                            let index = tree.put(current_rule_parent, block);
                            
                            rules_to_process.push_front((Rules::StmtToDo, index));
                            break 'inner;
                        }

                        tokens::If => 'inner: {
                            // if ( condition ) stmt elif
                            let block = TreeNode::child(current_rule_parent, Rules::StmtToIf, Value::Null);
                            let index = tree.put(current_rule_parent, block);
                            
                            rules_to_process.push_front((Rules::StmtToIf, index));
                            break 'inner;
                        }

                        tokens::Id(v) => 'inner: {
                            // expr
                            let block = TreeNode::child(current_rule_parent, Rules::StmtToExpr,table.get(v.to_string()).unwrap());
                            let index = tree.put(current_rule_parent, block);
                            
                            rules_to_process.push_front((Rules::StmtToIf, index));
                            break 'inner;
                        }

                        _ => {
                            // not a statement
                            println!("Found no valid statement {:?} with rule list {:?} at index {}", current_rule, rules_to_process, current_input_index);
                        }
                    }

                    break 'outer;

                },

                
                Rules::StmtToDecl => {
                    // inject basic var ;
                    // then deal with var
                    
                    // make blocks
                    let basic = TreeNode::child(current_rule_parent, Rules::Terminal(input[current_input_index].clone()), Value::Null);
                    let var = TreeNode::child(current_rule_parent, Rules::DeclToVar, Value::Null); 
                    let stop = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::EndEnclose), Value::Null);

                    //inject into parent
                    tree.put(current_rule_parent, basic);
                    let var_index = tree.put(current_rule_parent, var);
                    tree.put(current_rule_parent, stop);


                    // push next rule
                    rules_to_process.push_front((Rules::DeclToVar, var_index));
                    current_input_index += 1; // advance passed basic
                    break;
                },

                Rules::StmtToExpr => {
                    // apply stmt -> expr
                    
                    // make blocks
                    let expr = TreeNode::child(current_rule_parent, Rules::ExprToAssignEquation, Value::Null); 

                    //inject into parent
                    let expr_index = tree.put(current_rule_parent, expr);


                    // push next rule
                    rules_to_process.push_front((Rules::ExprToAssignEquation, expr_index));
                    // do not consume anything
                    break;
                },
                Rules::StmtToBreak => {
                    // make blocks
                    let basic = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Break), Value::Null);
                    let stop = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Stop), Value::Null);

                    //inject into parent
                    tree.put(current_rule_parent, basic);
                    tree.put(current_rule_parent, stop);


                    // push next rule
                    current_input_index += 2; // advance passed stop
                    break;
                },
                Rules::StmtToWhile => {
                    // while ( condition ) stmt
                    // 2 recurses here. Recurse block first then condition so block is deeper in the heep
                    
                    // specify this one because it goes last and is more general
                    rules_to_process.push_front((Rules::StmtToBlock, current_rule_parent));

                    // nonterminals
                    let whil = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::While), Value::Null);
                    let open = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Open), Value::Null);
                    let close = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Close), Value::Null);

                    // condition
                    let next_cond = Self::detect_next_conditon_type(&input,current_input_index + 2);
                    let cond = TreeNode::child(current_rule_parent, next_cond.clone(), Value::Null);

                    // add to tree
                    tree.put(current_rule_parent, whil);
                    tree.put(current_rule_parent, open);
                    let condition = tree.put(current_rule_parent, cond);
                    tree.put(current_rule_parent, close);

                    // push next rule
                    rules_to_process.push_front((next_cond, condition));
                    
                    // consume while and open
                    current_input_index += 2;
                    break;
                },
                Rules::StmtToDo => {
                    // do stmt while ( condition ) ;

                    // build terminals
                    let do_term = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Do), Value::Null);
                    let while_term = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::While), Value::Null);
                    let open = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Open), Value::Null);
                    let close = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Close), Value::Null);
                    let stop = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Stop), Value::Null);

                    // build stmt
                    let stmt = TreeNode::child(current_rule_parent, Rules::StmtMarker, Value::Null);

                    // build condition
                    let cond = TreeNode::child(current_rule_parent, Rules::ConditionMarker, Value::Null);


                    // add to parent in the right order
                    tree.put(current_rule_parent, do_term);
                    let stmt_index = tree.put(current_rule_parent, stmt);
                    tree.put(current_rule_parent, while_term);
                    tree.put(current_rule_parent, open);
                    let cond_index = tree.put(current_rule_parent, cond);
                    tree.put(current_rule_parent, close);
                    tree.put(current_rule_parent, stop);


                    // oddities here being the ; after condition. condition doesnt have a way to deal with that so we jump past it
                    rules_to_process.push_front((Rules::Jump(1), current_input_index));
                    // then add condition to the stack
                    rules_to_process.push_front((Rules::ConditionMarker, cond_index));
                    // add jump from end statement to condition
                    rules_to_process.push_front((Rules::Jump(2), current_input_index));
                    // add stmt to block
                    rules_to_process.push_front((Rules::StmtMarker, stmt_index));
                    break;
                },
                Rules::StmtToIf => {
                    // if ( condition ) stmt elif
                    // make all terminals
                    let if_term = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::If), Value::Null);
                    let open = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Open), Value::Null);
                    let close = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Close), Value::Null);


                    // make all non terminals
                    let cond = TreeNode::child(current_rule_parent, Rules::ConditionMarker, Value::Null);
                    let stmt = TreeNode::child(current_rule_parent, Rules::StmtMarker, Value::Null);
                    let elif = TreeNode::child(current_rule_parent, Rules::ElifMarker, Value::Null);


                    // add nodes in order to parent
                    tree.put(current_rule_parent, if_term);
                    tree.put(current_rule_parent, open);
                    let cond_index = tree.put(current_rule_parent, cond);
                    tree.put(current_rule_parent, close);
                    let stmt_index = tree.put(current_rule_parent, stmt);
                    let elif_index = tree.put(current_rule_parent, elif);

                    // add elif then stmt then condition into the stack
                    rules_to_process.push_front((Rules::ElifMarker, elif_index));
                    rules_to_process.push_front((Rules::ElifMarker, stmt_index));
                    rules_to_process.push_front((Rules::ElifMarker, cond_index));
                    break;
                },

                Rules::DeclToVar => {
                    // detect type of var
                    if input[current_input_index+1] == tokens::Stop {
                        // id
                        
                        // make blocks 
                        let var = TreeNode::child(current_rule_parent, Rules::VarToID, Value::Null); 
                        let id = TreeNode::child(current_rule_parent, Rules::Terminal(input[current_input_index].clone()), Value::Null);
                        
                        // inject
                        let var_index = tree.put(current_rule_parent, var);
                        tree.put(var_index, id);

                        // advace past ;
                        current_input_index += 2; // advance 2 tokens


                    } else {
                        // id []
                        
                        // make blocks 
                        let var = TreeNode::child(current_rule_parent, Rules::VarToIDArray, Value::Null); 
                        let id = TreeNode::child(current_rule_parent, Rules::Terminal(input[current_input_index].clone()), Value::Null);
                        let array = TreeNode::child(current_rule_parent, Rules::Terminal(input[current_input_index+1].clone()), Value::Null);
                        
                        // inject
                        let var_index = tree.put(current_rule_parent, var);
                        tree.put(var_index, id);
                        tree.put(var_index, array);

                        // advace past ;
                        current_input_index += 3; // advance 3 tokens
                    }
                    break;
                },

                Rules::ExprToAssignEquation => {
                    // apply expr -> var = equation ;
                    let mut var_offset = 0;

                    if !Self::detect_id_array(&input, current_rule_parent) {
                        // var is id
                        let var = TreeNode::child(current_rule_parent, Rules::VarToID, Value::Null); 
                        let id = TreeNode::child(current_rule_parent, Rules::Terminal(input[current_input_index].clone()), Value::Null);
                        let var_index = tree.put(current_rule_parent, var);
                        tree.put(var_index, id);

                        var_offset = 1;
                    } else {
                        // var is id array
                        let var = TreeNode::child(current_rule_parent, Rules::VarToIDArray, Value::Null); 
                        let id = TreeNode::child(current_rule_parent, Rules::Terminal(input[current_input_index].clone()), Value::Null);
                        let array = TreeNode::child(current_rule_parent, Rules::Terminal(input[current_input_index+1].clone()), Value::Null);
                        
                        // inject
                        let var_index = tree.put(current_rule_parent, var);
                        tree.put(var_index, id);
                        tree.put(var_index, array);
                        var_offset = 2;
                    }

                    // detect type of equation
                    let equation_type = Self::detect_next_equation_type(&input,current_input_index+var_offset); // is never heq


                    // make rest of the blocks
                    let equation = TreeNode::child(current_rule_parent, equation_type.clone(), Value::Null); 
                    let stop = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Stop), Value::Null); 

                    //inject
                    let eq_index = tree.put(current_rule_parent, equation);
                    tree.put(current_rule_parent, stop);


                    // push next rule
                    rules_to_process.push_front((equation_type, eq_index));

                    // advance to start of equation
                    current_input_index += var_offset;
                },

                // deal with every equation
               Rules::EquationToAddition | Rules::EquationToSubtraction | Rules::EquationToMultiplication | Rules::EquationToDevision => {
                    // get output
                    let output_tuple = Self::compile_next_object(&input, current_input_index, &mut tree, current_rule_parent);
                    // this puts output in the parent for me
                    
                    let opperator:TreeNode;
                    match current_rule {
                        Rules::EquationToAddition => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Plus), Value::Null); 
                        }
                        Rules::EquationToSubtraction => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Minus), Value::Null); 
                        }
                        Rules::EquationToMultiplication => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Times), Value::Null); 
                        }
                        Rules::EquationToDevision => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Devides), Value::Null); 
                        }
                        _ =>{
                            panic!()
                        } 
                    }

                    // connect opperator
                    tree.put(current_rule_parent, opperator);
                    
                    // find next type 
                    let next_type = Self::detect_next_equation_type(&input, output_tuple.0+1);
                    let next_equation = TreeNode::child(current_rule_parent, next_type.clone(), Value::Null);
                    let equation_id = tree.put(current_rule_parent, next_equation);

                    rules_to_process.push_front((next_type, equation_id)); 
                    

                    // accorunt for opperator
                    current_input_index = output_tuple.0 + 1;
                    break;
                },

                Rules::EquationToEncloseHeq => {
                    rules_to_process.push_front((Rules::HeqMarker, current_rule_parent));
                    // push the heq marker there so that when the enclose is done it backs out and tries the Heq

                    //make all tterminalblocks
                    let start_pren = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Open), Value::Null);
                    let end_pren = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Close), Value::Null);


                    
                    // detrermin equation type
                    let eq_type = Self::detect_next_equation_type(&input, current_input_index+1); //+1 added to get inside the ()
                    let equation = TreeNode::child(current_rule_parent, eq_type.clone(), Value::Null);


                    //inject into parent
                    tree.put(current_rule_parent, start_pren);
                    let equation_index = tree.put(current_rule_parent, equation);
                    tree.put(current_rule_parent, end_pren);

                    // push next rule
                    rules_to_process.push_front((eq_type, equation_index));
                    current_input_index += 1; // advance 1 token into the block
                    break;

                },

                Rules::EquationToObject => {
                    // end of the line
                    let output_tuple = Self::compile_next_object(&input, current_input_index, &mut tree, current_rule_parent);
                    // account for object and stop or close
                    current_input_index = output_tuple.0 + 1;
                },

                Rules::HeqMarker => {
                    //heq found! is there anything here?
                    match input[current_input_index]{
                        tokens::Plus => {
                            rules_to_process.push_front((Rules::HeqToAdd, current_rule_parent));
                            break;
                        }
                        tokens::Minus => {
                            rules_to_process.push_front((Rules::HeqToSubtract, current_rule_parent));
                            break;
                        }
                        tokens::Times => {
                            rules_to_process.push_front((Rules::HeqToMultiply, current_rule_parent));
                            break;
                        }
                        tokens::Devides => {
                            rules_to_process.push_front((Rules::HeqToDevide, current_rule_parent));
                            break;
                        }

                        _ => {
                            // nothing found. Epsilon and leave
                            rules_to_process.push_front((Rules::HeqToEpsilon, current_rule_parent));
                            break;
                        }
                    }
                }

                // deal with all heq's Theres only like 1 diffrence and thats the begining lol
                Rules::HeqToAdd | Rules::HeqToSubtract | Rules::HeqToMultiply | Rules::HeqToDevide => {
                    // get opperayor
                    let opperator:TreeNode;
                    match current_rule {
                        Rules::HeqToAdd => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Plus), Value::Null); 
                        }
                        Rules::HeqToSubtract => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Minus), Value::Null); 
                        }
                        Rules::HeqToMultiply => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Times), Value::Null); 
                        }
                        Rules::HeqToDevide => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Devides), Value::Null); 
                        }
                        _ =>{
                            // cant be nothing bc you have to go through marker to get here
                            panic!()
                        } 
                    }

                    // connect opperator
                    tree.put(current_rule_parent, opperator);
                    
                    // find next type 
                    let next_type = Self::detect_next_equation_type(&input, current_input_index+1);
                    let next_equation = TreeNode::child(current_rule_parent, next_type.clone(), Value::Null);
                    let equation_id = tree.put(current_rule_parent, next_equation);

                    rules_to_process.push_front((next_type, equation_id)); 
                    

                    // accorunt for opperator and enter the equation
                    current_input_index += 1;
                    break;
                }


                Rules::ConditionMarker => {
                    //catch type of condition and then exicute it.
                    let next_cond = Self::detect_next_conditon_type(&input, current_input_index);
                    rules_to_process.push_front((next_cond,current_rule_parent)); // go do it :3
                },
                Rules::ConditionToEquals | Rules::ConditionToNotEquals | Rules::ConditionToLess | Rules::ConditionToLessEq | Rules::ConditionToMore | Rules::ConditionToMoreEq | Rules::ConditionToAnd | Rules::ConditionToOr => {
                    // object op condition
                    // fist compile object
                    let object_out = Self::compile_next_object(&input, current_input_index, &mut tree, current_rule_parent);

                    let opperator:TreeNode;
                    match current_rule {
                        Rules::ConditionToEquals => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Equals), Value::Null); 
                        }
                        Rules::ConditionToNotEquals => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Neq), Value::Null); 
                        }
                        Rules::ConditionToLess => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Les), Value::Null); 
                        }
                        Rules::ConditionToLessEq => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Leq), Value::Null); 
                        }
                        Rules::ConditionToMore => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Gre), Value::Null); 
                        }
                        Rules::ConditionToMoreEq => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Geq), Value::Null); 
                        }
                        Rules::ConditionToAnd => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::And), Value::Null); 
                        }
                        Rules::ConditionToOr => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Or), Value::Null); 
                        }
                        _ =>{
                            panic!()
                        }
                    }

                    // connect opperator
                    tree.put(current_rule_parent, opperator);
                    
                    // find next type 
                    let next_type = Self::detect_next_conditon_type(&input, object_out.0+1); // start detecting after current opperator
                    let next_equation = TreeNode::child(current_rule_parent, next_type.clone(), Value::Null);
                    let equation_id = tree.put(current_rule_parent, next_equation);

                    rules_to_process.push_front((next_type, equation_id)); 
                    

                    // account for opperator
                    current_input_index = object_out.0 + 1;
                },
                Rules::ConditionToEnclosed => {
                    rules_to_process.push_front((Rules::HconMarker, current_rule_parent));
                    // push the hcon marker there so that when the enclose is done it backs out and tries the Heq

                    //make all tterminalblocks
                    let start_pren = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Open), Value::Null);
                    let end_pren = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Close), Value::Null);


                    
                    // detrermin condition type
                    let con_type = Self::detect_next_conditon_type(&input, current_input_index+1); //+1 added to get inside the ()
                    let cond = TreeNode::child(current_rule_parent, con_type.clone(), Value::Null);


                    //inject into parent
                    tree.put(current_rule_parent, start_pren);
                    let cond_index = tree.put(current_rule_parent, cond);
                    tree.put(current_rule_parent, end_pren);

                    // push next rule
                    rules_to_process.push_front((con_type, cond_index));
                    current_input_index += 1; // advance 1 token into the block
                    break;
                },
                Rules::ConditionToObject => {
                    // end of the condition
                    let output_tuple = Self::compile_next_object(&input, current_input_index, &mut tree, current_rule_parent);
                    // account for object and close
                    current_input_index = output_tuple.0 + 1;
                },

                Rules::HconMarker => {
                    //Hcon found! is there anything here?
                    match input[current_input_index] {
                        tokens::Equals =>{
                            rules_to_process.push_front((Rules::HconToEquals, current_rule_parent));
                        }
                        tokens::Neq =>{
                            rules_to_process.push_front((Rules::HconToNoteq, current_rule_parent));
                        }
                        tokens::Les =>{
                            rules_to_process.push_front((Rules::HconToLess, current_rule_parent));
                        }
                        tokens::Leq =>{
                            rules_to_process.push_front((Rules::HconToLessEq, current_rule_parent));
                        }
                        tokens::Gre =>{
                            rules_to_process.push_front((Rules::HconToMore, current_rule_parent));
                        }
                        tokens::Geq =>{
                            rules_to_process.push_front((Rules::HconToMoreEq, current_rule_parent));
                        }
                        tokens::And =>{
                            rules_to_process.push_front((Rules::HconToAnd, current_rule_parent));
                        }
                        tokens::Or =>{
                            rules_to_process.push_front((Rules::HconToOr, current_rule_parent));
                        }
                        _ => {
                            // nothing transition to epsilon
                            rules_to_process.push_front((Rules::HconToEpsilon, current_rule_parent));
                        }
                    }
                }
                
                // deal with all of hcon
                Rules::HconToEquals | Rules::HconToNoteq | Rules::HconToLess | Rules::HconToLessEq | Rules::HconToMore | Rules::HconToMoreEq | Rules::HconToAnd | Rules::HconToOr => {
                    // get opperayor
                    let opperator:TreeNode;
                    match current_rule {
                        Rules::HconToEquals => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Equals), Value::Null); 
                        }
                        Rules::HconToNoteq => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Equals), Value::Null); 
                        }
                        Rules::HconToLess => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Les), Value::Null); 
                        }
                        Rules::HconToLessEq => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Leq), Value::Null); 
                        }
                        Rules::HconToMore => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Gre), Value::Null); 
                        }
                        Rules::HconToMoreEq => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Geq), Value::Null); 
                        }
                        Rules::HconToAnd => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::And), Value::Null); 
                        }
                        Rules::HconToOr => {
                            opperator = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Or), Value::Null); 
                        }
                        _ =>{
                            panic!()
                        }
                    }

                    // connect opperator
                    tree.put(current_rule_parent, opperator);
                    
                    // find next type 
                    let next_type = Self::detect_next_conditon_type(&input, current_input_index+1); // start detecting after current opperator
                    let next_equation = TreeNode::child(current_rule_parent, next_type.clone(), Value::Null);
                    let equation_id = tree.put(current_rule_parent, next_equation);

                    rules_to_process.push_front((next_type, equation_id));
                    

                    // account for opperator
                    current_input_index = current_input_index + 1;
                    break;
                }




                Rules::ElifMarker => {
                    // 1 determine if there is an else
                    if input[current_input_index] == tokens::Else {
                        // well. Theres an else it seams
                        rules_to_process.push_front((Rules::ElifToElse,current_rule_parent)); // go do it :3
                    } else {
                        // guesss its nothing
                        rules_to_process.push_front((Rules::ElifToEpsilon,current_rule_parent));
                    }
                },
                Rules::ElifToElse => {
                    // else stmt
                    
                    // add in terminal else and then go to stmt block
                    let els = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Else), Value::Null); 
                    tree.put(current_rule_parent, els);

                    // stmt
                    rules_to_process.push_front((Rules::StmtMarker, current_rule_parent));

                    current_input_index += 1; // advance 1 token
                    break;

                },


                // deal with epsilon
                Rules::BlockToEpsilon  => {
                    // add in the terminal epsilon
                    // only happens when encountered } in block to statement block
                    // dont do anything just go past
                    let epsilon = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Null), Value::Null); 
                    tree.put(current_rule_parent, epsilon);

                    current_input_index += 1; // advance 1 token
                    break;
                },
                Rules::ElifToEpsilon => {
                    // add in the terminal epsilon
                    // only happens when if statement has ended. no clue what could be next so dont do anything
                    // dont do anything just go past
                    let epsilon = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Null), Value::Null); 
                    tree.put(current_rule_parent, epsilon);

                    // do not advance any tokens
                    break;
                },

                Rules::HeqToEpsilon | Rules::HconToEpsilon => {
                    // nothings here and dont do anything catch put in epsilon and move on
                    let epsilon = TreeNode::child(current_rule_parent, Rules::Terminal(tokens::Null), Value::Null); 
                    tree.put(current_rule_parent, epsilon);

                    break;
                }

                Rules::Jump(n) =>{
                    current_input_index += n;
                    break;
                }


                // error
                Rules::Terminal(_) => todo!(),
                _ => {
                    println!("Encountered rule not accounted for. {:?} with rule list {:?} at index {}", current_rule, rules_to_process, current_input_index)
                }
            }
        
            // to to next rule in the heep
            if !rules_to_process.is_empty() {
                let next= rules_to_process.pop_front().unwrap();
                current_rule = next.0;
                current_rule_parent = next.1;
                
            }
            else if current_input_index <= input.len() {
                // ran out of rules and not tokens
                println!("ran out of rules. {:?} with rule list {:?} at index {}", current_rule, rules_to_process, current_input_index)
            }
        }
        return Ok(tree);
    }

    fn detect_next_equation_type(input: &Vec<tokens>, offset:usize) -> Rules{
        for token in &input[offset..]{
            match token {
                tokens::Stop | tokens::Close=>{
                    return Rules::EquationToObject; // end of this equation
                }
                tokens::Plus =>{
                    return Rules::EquationToAddition;
                }
                tokens::Minus =>{
                    return Rules::EquationToSubtraction;
                }
                tokens::Times =>{
                    return Rules::EquationToMultiplication;
                }
                tokens::Devides =>{
                    return Rules::EquationToDevision;
                }
                tokens::Open =>{
                    return Rules::EquationToEncloseHeq;
                }
                _ => {
                    continue;
                }
            }
        }
        panic!(); // AHHHHHHHHHHHHHHH
    }

    // returns new offset node and the position of the newly created object
    fn compile_next_object(input: &Vec<tokens>, offset:usize, tree: &mut TreeControler, parent:usize) -> (usize, usize){
        for token in &input[offset..]{
            match token {
                tokens::Id(_) => {
                    // detet type of id
                    if !Self::detect_id_array(input,offset){
                        let object = TreeNode::child(parent, Rules::ObjectToVar, Value::Null);
                        let var = TreeNode::child(parent, Rules::VarToID, Value::Null); 
                        let id = TreeNode::child(parent, Rules::Terminal(input[offset].clone()), Value::Null);

                        let object_index = tree.put(parent, object);
                        let var_index = tree.put(object_index, var);
                        tree.put(var_index, id);
                        return (offset+1, object_index);
                    } else {
                        let object = TreeNode::child(parent, Rules::ObjectToVar, Value::Null);
                        let var = TreeNode::child(parent, Rules::VarToIDArray, Value::Null); 
                        let id = TreeNode::child(parent, Rules::Terminal(input[offset].clone()), Value::Null);
                        let array = TreeNode::child(parent, Rules::Terminal(input[offset+1].clone()), Value::Null);

                        let object_index = tree.put(parent, object);
                        let var_index = tree.put(object_index, var);
                        tree.put(var_index, id);
                        tree.put(var_index, array);
                        
                        return (offset+2, object_index);
                    }
                }
                tokens::Number(_) | tokens::Real(_) | tokens::True | tokens::False => {
                    // object type
                    let obj_type: Rules;
                    match input[offset] {
                        tokens::Number(_) => {obj_type = Rules::ObjectToNum}
                        tokens::Real(_) => {obj_type = Rules::ObjectToReal}
                        tokens::True => {obj_type = Rules::ObjectToTrue}
                        tokens::False => {obj_type = Rules::ObjectToFalse}
                        _ =>{panic!()}
                    }


                    let object = TreeNode::child(parent, obj_type, Value::Null);
                    let terminal = TreeNode::child(parent, Rules::Terminal(input[offset].clone()), Value::Null); 

                    let object_index = tree.put(parent, object);
                    tree.put(object_index, terminal);
                    return (offset+1, object_index);
                }
                _ => {
                    // nothing yet
                }
            }
        }
        panic!();
    }

    fn detect_id_array(input: &Vec<tokens>, offset:usize) -> bool{
        match input[offset+1] {
            tokens::Array(_, _) => {
                return true;
            }
            tokens::GetValue(_, _) => {
                return true;
            }
            _ => {
                return false;
            }
        }
    }

    fn detect_next_conditon_type(input: &Vec<tokens>, offset:usize) -> Rules{
        match input[offset] {
            tokens::Close=>{
                return Rules::ConditionToObject;
            }
            tokens::Equals =>{
                return Rules::ConditionToEquals;
            }
            tokens::Neq =>{
                return Rules::ConditionToNotEquals;
            }
            tokens::Les =>{
                return Rules::ConditionToLess;
            }
            tokens::Leq =>{
                return Rules::ConditionToLessEq;
            }
            tokens::Gre =>{
                return Rules::ConditionToMore;
            }
            tokens::Geq =>{
                return Rules::ConditionToMoreEq;
            }
            tokens::And =>{
                return Rules::ConditionToAnd;
            }
            tokens::Or =>{
                return Rules::ConditionToOr;
            }
            tokens::Open => {
                return Rules::ConditionToEnclosed;
            }
            _ => {
                panic!();
            }
        }
    } 
}