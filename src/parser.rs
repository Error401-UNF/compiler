//parser.rs

use std::collections::{VecDeque};
use std::fmt::format;

use super::lexical_analyzer::{tokens};
use super::symbol_table::{Env, Value};




#[derive(Debug,Clone)]
pub struct TreeNode {
    parent: Option<usize>,
    this_node: Option<usize>,
    children: Vec<usize>,
    rule:Rules,
    valuetype: Value
}

impl TreeNode {
    fn program() -> Self{
        TreeNode{
            parent: None,
            this_node: Some(0),
            children: vec![],
            rule: Rules::ProgramToBlock,
            valuetype: Value::Null,
        }
    }
    fn child(parent_index:usize, rule:Rules,value:Value) -> Self{
        TreeNode{
            parent: Some(parent_index),
            this_node: None,
            children: vec![],
            rule: rule,
            valuetype: value,
        }
    }
    fn render(self) -> String{
        // print self.
        return format!("{:?}",self);
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

    pub fn put(&mut self, parent_index: usize, mut child:TreeNode) -> usize{
        let child_position = self.tree_vector.len();
        let parent = self.get(parent_index);

        if parent.is_some(){
            let mut_parent = parent.unwrap();
            mut_parent.children.push(child_position);
            child.this_node = Some(child_position);
            self.tree_vector.push(child);
        }        
        return child_position;
    }

    pub fn render_node(&self, node_index:usize){
        // render self
        println!("{}",self.tree_vector[node_index].clone().render());

        // render children
        for child in self.tree_vector[node_index].clone().children{
            Self::render_node(self, child);
        }
    }

    fn update_marker(&mut self, marker_index:usize, new_rule:Rules) {
        // could work for anything but i am only using these for markers lmao
        let marker_node = Self::get(self, marker_index);
        if marker_node.is_some() {
            let marker = marker_node.unwrap();
            marker.rule = new_rule;
        }
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

    // arr rules
    ArrMarker,
    ArrToArrayArr,
    ArrToEpsilon,

    // var rules
    VarToIDArr,
    
    // expr rules
    ExprToAssignEquation,

    // equation rules
    EquationMarker,
    EquationToObjectHeq,
    EquationToEncloseHeq,
    EquationToNegate,

    //heq rules
    HeqMarker,
    HeqToAdd,
    HeqToSubtract,
    HeqToMultiply,
    HeqToDevide,
    HeqToEpsilon,
    HeqToHcon,

    // object rules
    ObjectToVar,
    ObjectToNum,
    ObjectToReal,
    ObjectToTrue,
    ObjectToFalse,

    // condition rules
    ConditionMarker,
    ConditionToObjectHcon,
    ConditionToEnclosedHcon,
    ConditionToNegate,


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
    HconToHeq,

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
            Rules::DeclToVar => { return "decl -> basic arr var ;".to_string()},
            Rules::VarToIDArr => { return "var -> id arr".to_string()},

            Rules::ExprToAssignEquation => { return "expr -> var = equation ;".to_string()},

            Rules::EquationToEncloseHeq => { return "equation -> ( equation )".to_string()},
            Rules::EquationToObjectHeq => { return "equation -> object heq".to_string()},
            Rules::EquationToNegate => { return "equation -> - equation".to_string()},

            Rules::ObjectToVar => { return "object -> var".to_string()},
            Rules::ObjectToNum => { return "object -> num".to_string()},
            Rules::ObjectToReal => { return "object -> real".to_string()},
            Rules::ObjectToTrue => { return "object -> true".to_string()},
            Rules::ObjectToFalse => { return "object -> false".to_string()},

            Rules::ConditionToEnclosedHcon => { return "condition -> ( condition )".to_string()},
            Rules::ConditionToObjectHcon => { return "condition -> object".to_string()},
            Rules::ConditionToNegate => { return "condition -> ! condition".to_string()},

            Rules::ElifToElse => { return "elif -> else stmt".to_string()},
            Rules::ElifToEpsilon => { return "elif -> ε".to_string()},

            Rules::HeqToAdd =>  { return "heq -> + equation".to_string()},
            Rules::HeqToSubtract => { return "heq -> - equation".to_string()},
            Rules::HeqToMultiply => { return "heq -> * equation".to_string()},
            Rules::HeqToDevide => { return "heq -> / equation".to_string()},
            Rules::HeqToEpsilon => { return "heq -> ε".to_string()},
            Rules::HeqToHcon => { return "heq -> hcon".to_string()},
            
            Rules::HconToEquals => { return "hcon -> == condition".to_string()},
            Rules::HconToNoteq =>  { return "hcon -> != condition".to_string()},
            Rules::HconToLess =>  { return "hcon -> < condition".to_string()},
            Rules::HconToLessEq =>  { return "hcon -> <= condition".to_string()},
            Rules::HconToMore =>  { return "hcon -> > condition".to_string()},
            Rules::HconToMoreEq =>  { return "hcon -> >= condition".to_string()},
            Rules::HconToAnd =>  { return "hcon -> && condition".to_string()},
            Rules::HconToOr =>  { return "hcon -> || condition".to_string()},
            Rules::HconToEpsilon =>  { return "hcon -> ε".to_string()},
            Rules::HconToHeq => { return "hcon -> heq".to_string()},
            
            Rules::ArrToArrayArr => { return "arr -> [ condition ] arr".to_string()},
            Rules::ArrToEpsilon => { return "arr -> ε".to_string()},

            Rules::StmtMarker =>  { return "stmt marker".to_string()},
            Rules::EquationMarker => { return "Equation marker".to_string()},
            Rules::HeqMarker => { return "heq marker".to_string()},
            Rules::ConditionMarker => { return "Condition marker".to_string()},
            Rules::HconMarker => { return "hcon marker".to_string()},
            Rules::ElifMarker => { return "elif marker".to_string()},
            Rules::ArrMarker => { return "Arr marker".to_string()},

            Rules::Terminal(_) => { return "".to_string()},
            Rules::Jump(d) => { return format!("Skip {} spaces", d )},

            r => {return format!("Not Implemented {:?}", r);}
        }
    }
}



pub struct Parser{
    
}

impl Parser {
    pub fn parse(input: Vec<tokens>, table:Env) -> Result<TreeControler, String>{
        let mut current_input_index:usize = 0;
        let mut tree = TreeControler::new();
        let mut current_rule = Rules::ProgramToBlock;
        let mut current_rule_Node:usize = 0;
        let mut rules_to_process: VecDeque<(Rules, usize)> = VecDeque::new(); // acting as a heap


        println!("Parsing started.");
        println!("Currently executing rule {:?} whos parent is {} at index {}", current_rule.render(), current_rule_Node ,current_input_index);

        while current_input_index <= input.len(){

            match current_rule  {

                // deal with blocks
                Rules::ProgramToBlock | Rules::StmtToBlock =>  'rule_check:{
                    //rule only encountered in 2 sinarios. Start of program. or block that looks like this { block }
                    // either way it makes a block that looks like thiis { block } so only deal with that and error otherwise
                    
                    
                    //make all blocks
                    let start_enclose = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::StartEnclose), Value::Null);
                    let stmt_block = TreeNode::child(current_rule_Node, Rules::BlockToStmtBlock, Value::Null); 
                    let end_enclose = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::EndEnclose), Value::Null);

                    //inject into parent
                    tree.put(current_rule_Node, start_enclose);
                    let stmt_index = tree.put(current_rule_Node, stmt_block);
                    tree.put(current_rule_Node, end_enclose);

                    // push next rule
                    rules_to_process.push_front((Rules::Jump(1), stmt_index));

                    rules_to_process.push_front((Rules::BlockToStmtBlock, stmt_index));
                    current_input_index += 1; // advance 1 token
                    break  'rule_check;
                },

                // specal combination. Statement block deals withe verything block
                Rules::BlockToStmtBlock  => 'outer: {
                    // all this rule is saying is that we are allowing 2 statements in a row.
                    // logic: use this to deal with a statement (something ending with ';')
                    // logic: detect } symbol to end block and return epsilon


                    // recurse. placed first so its exicuted after whatever is inside the statement.
                    rules_to_process.push_front((Rules::BlockToStmtBlock, current_rule_Node));

                    match &input[current_input_index] {
                        tokens::EndEnclose => {
                            // remove the recursion
                            rules_to_process.pop_front();

                            // epsilon
                            rules_to_process.push_front((Rules::BlockToEpsilon, current_rule_Node));
                            break 'outer; // break to outer match
                        }

                        // detect all types of statements
                        tokens::StartEnclose => 'inner: {
                            // { block }
                            let stmt_block = TreeNode::child(current_rule_Node, Rules::StmtToBlock, Value::Null);
                            let stmt_index = tree.put(current_rule_Node, stmt_block);
                            rules_to_process.push_front((Rules::BlockToStmtBlock, stmt_index));
                            current_input_index += 1; // advance 1 token
                            break 'inner;
                        }

                        tokens::Basic(v) => 'inner: {
                            // decl
                            let decl_block = TreeNode::child(current_rule_Node, Rules::StmtToDecl, v.clone());
                            let decl_index = tree.put(current_rule_Node, decl_block);
                            
                            rules_to_process.push_front((Rules::StmtToDecl, decl_index));
                            break 'inner;
                        }

                        tokens::Break => 'inner: {
                            // break ;
                            let break_block = TreeNode::child(current_rule_Node, Rules::StmtToBreak,  Value::Null);
                            let break_index = tree.put(current_rule_Node, break_block);
                            
                            rules_to_process.push_front((Rules::StmtToBreak, break_index));
                            break 'inner;
                        }

                        tokens::While => 'inner: {
                            // while ( condition ) { block }
                            let block = TreeNode::child(current_rule_Node, Rules::StmtToWhile,  Value::Null);
                            let index = tree.put(current_rule_Node, block);
                            
                            rules_to_process.push_front((Rules::StmtToWhile, index));
                            break 'inner;
                        }

                        tokens::Do => 'inner: {
                            // do stmt while ( condition ) ;
                            let block = TreeNode::child(current_rule_Node, Rules::StmtToDo, Value::Null);
                            let index = tree.put(current_rule_Node, block);
                            
                            rules_to_process.push_front((Rules::StmtToDo, index));
                            break 'inner;
                        }

                        tokens::If => 'inner: {
                            // if ( condition ) stmt elif
                            let block = TreeNode::child(current_rule_Node, Rules::StmtToIf, Value::Null);
                            let index = tree.put(current_rule_Node, block);
                            
                            rules_to_process.push_front((Rules::StmtToIf, index));
                            break 'inner;
                        }

                        tokens::Id(v) => 'inner: {
                            // expr
                            let block = TreeNode::child(current_rule_Node, Rules::StmtToExpr,table.get(v.to_string()).unwrap());
                            let index = tree.put(current_rule_Node, block);
                            
                            rules_to_process.push_front((Rules::StmtToExpr, index));
                            break 'inner;
                        }

                        _ => {
                            // not a statement
                            println!("Found no valid statement {:?} with rule list {:?} at index {}. Problem Symbol: {:?}", current_rule, rules_to_process, current_input_index, &input[current_input_index]);
                            return Err(format!("Failed to parse :( {:?} with rule list {:?} at index {}", current_rule, rules_to_process, current_input_index))
                        }
                    }

                    break 'outer;

                },

                Rules::StmtMarker => 'outer: {
                    // all this rule is saying is that we are allowing 2 statements in a row.
                    // logic: use this to deal with a statement (something ending with ';')
                    // logic: detect } symbol to end block and return epsilon

                    match &input[current_input_index] {

                        // detect all types of statements
                        tokens::StartEnclose => 'inner: {
                            // { block }
                            let stmt_block = TreeNode::child(current_rule_Node, Rules::StmtToBlock, Value::Null);
                            let stmt_index = tree.put(current_rule_Node, stmt_block);

                            tree.update_marker(current_rule_Node, Rules::StmtToBlock);
                            rules_to_process.push_front((Rules::StmtToBlock, stmt_index));
                            break 'inner;
                        }

                        tokens::Basic(v) => 'inner: {
                            // decl
                            let decl_block = TreeNode::child(current_rule_Node, Rules::StmtToDecl, v.clone());
                            let decl_index = tree.put(current_rule_Node, decl_block);
                            
                            tree.update_marker(current_rule_Node, Rules::StmtToDecl);
                            rules_to_process.push_front((Rules::StmtToDecl, decl_index));
                            break 'inner;
                        }

                        tokens::Break => 'inner: {
                            // break ;
                            let break_block = TreeNode::child(current_rule_Node, Rules::StmtToBreak,  Value::Null);
                            let break_index = tree.put(current_rule_Node, break_block);
                            
                            tree.update_marker(current_rule_Node, Rules::StmtToBreak);
                            rules_to_process.push_front((Rules::StmtToBreak, break_index));
                            break 'inner;
                        }

                        tokens::While => 'inner: {
                            // while ( condition ) { block }
                            let block = TreeNode::child(current_rule_Node, Rules::StmtToWhile,  Value::Null);
                            let index = tree.put(current_rule_Node, block);
                            
                            tree.update_marker(current_rule_Node, Rules::StmtToWhile);
                            rules_to_process.push_front((Rules::StmtToWhile, index));
                            break 'inner;
                        }

                        tokens::Do => 'inner: {
                            // do stmt while ( condition ) ;
                            let block = TreeNode::child(current_rule_Node, Rules::StmtToDo, Value::Null);
                            let index = tree.put(current_rule_Node, block);
                            
                            tree.update_marker(current_rule_Node, Rules::StmtToDo);
                            rules_to_process.push_front((Rules::StmtToDo, index));
                            break 'inner;
                        }

                        tokens::If => 'inner: {
                            // if ( condition ) stmt elif
                            let block = TreeNode::child(current_rule_Node, Rules::StmtToIf, Value::Null);
                            let index = tree.put(current_rule_Node, block);
                            
                            tree.update_marker(current_rule_Node, Rules::StmtToIf);
                            rules_to_process.push_front((Rules::StmtToIf, index));
                            break 'inner;
                        }

                        tokens::Id(v) => 'inner: {
                            // expr
                            let block = TreeNode::child(current_rule_Node, Rules::StmtToExpr,table.get(v.to_string()).unwrap());
                            let index = tree.put(current_rule_Node, block);
                            
                            tree.update_marker(current_rule_Node, Rules::StmtToExpr);
                            rules_to_process.push_front((Rules::StmtToExpr, index));
                            break 'inner;
                        }

                        _ => {
                            // not a statement
                            println!("Found no valid statement {:?} with rule list {:?} at index {}. Problem Symbol: {:?}", current_rule, rules_to_process, current_input_index, &input[current_input_index]);
                            return Err(format!("Failed to parse :( {:?} with rule list {:?} at index {}", current_rule, rules_to_process, current_input_index))
                        }
                    }

                    break 'outer;

                },
                
                Rules::StmtToDecl =>  'rule_check: {
                    // inject basic var ;
                    // then deal with var
                    
                    // make blocks
                    let basic = TreeNode::child(current_rule_Node, Rules::Terminal(input[current_input_index].clone()), Value::Null);
                    let var = TreeNode::child(current_rule_Node, Rules::DeclToVar, Value::Null); 
                    let stop = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Stop), Value::Null);

                    //inject into parent
                    tree.put(current_rule_Node, basic);
                    let var_index = tree.put(current_rule_Node, var);
                    tree.put(current_rule_Node, stop);


                    // push next rule
                    rules_to_process.push_front((Rules::DeclToVar, var_index));
                    break  'rule_check;
                },

                Rules::StmtToExpr =>  'rule_check: {
                    // apply stmt -> expr
                    
                    // make blocks
                    let expr = TreeNode::child(current_rule_Node, Rules::ExprToAssignEquation, Value::Null); 

                    //inject into parent
                    let expr_index = tree.put(current_rule_Node, expr);


                    // push next rule

                    rules_to_process.push_front((Rules::ExprToAssignEquation, expr_index));
                    // do not consume anything
                    break  'rule_check;
                },
                Rules::StmtToBreak =>  'rule_check: {
                    // make blocks
                    let basic = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Break), Value::Null);
                    let stop = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Stop), Value::Null);

                    //inject into parent
                    tree.put(current_rule_Node, basic);
                    tree.put(current_rule_Node, stop);


                    // push next rule
                    current_input_index += 2; // advance passed stop
                    break  'rule_check;
                },
                Rules::StmtToWhile =>  'rule_check: {
                    // while ( condition ) stmt
                    // 2 recurses here. Recurse block first then condition so block is deeper in the heep
                    
                    // specify this one because it goes last and is more general

                    // nonterminals
                    let whil = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::While), Value::Null);
                    let open = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Open), Value::Null);
                    let close = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Close), Value::Null);

                    // condition
                    let next_cond = Self::detect_next_conditon_type(&input,current_input_index + 2);
                    let cond = TreeNode::child(current_rule_Node, next_cond.clone(), Value::Null);

                    // add to tree
                    tree.put(current_rule_Node, whil);
                    tree.put(current_rule_Node, open);
                    let condition = tree.put(current_rule_Node, cond);
                    tree.put(current_rule_Node, close);

                    // push next rule
                    rules_to_process.push_front((Rules::StmtMarker, current_rule_Node));
                    rules_to_process.push_front((Rules::Jump(1), condition)); // consume close )
                    rules_to_process.push_front((next_cond, condition));
                    
                    // consume while and open
                    current_input_index += 2;
                    break  'rule_check;
                },
                Rules::StmtToDo =>  'rule_check: {
                    // do stmt while ( condition ) ;

                    // build terminals
                    let do_term = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Do), Value::Null);
                    let while_term = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::While), Value::Null);
                    let open = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Open), Value::Null);
                    let close = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Close), Value::Null);
                    let stop = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Stop), Value::Null);

                    // build stmt
                    let stmt = TreeNode::child(current_rule_Node, Rules::StmtMarker, Value::Null);

                    // build condition
                    let cond = TreeNode::child(current_rule_Node, Rules::ConditionMarker, Value::Null);


                    // add to parent in the right order
                    tree.put(current_rule_Node, do_term);
                    let stmt_index = tree.put(current_rule_Node, stmt);
                    tree.put(current_rule_Node, while_term);
                    tree.put(current_rule_Node, open);
                    let cond_index = tree.put(current_rule_Node, cond);
                    tree.put(current_rule_Node, close);
                    tree.put(current_rule_Node, stop);


                    // oddities here being the ; after condition. condition doesnt have a way to deal with that so we jump past it
                    rules_to_process.push_front((Rules::Jump(2), stmt_index));
                    // then add condition to the stack
                    rules_to_process.push_front((Rules::ConditionMarker, cond_index));
                    // add jump from end statement to condition
                    rules_to_process.push_front((Rules::Jump(2), stmt_index));
                    // add stmt to block
                    rules_to_process.push_front((Rules::StmtMarker, stmt_index));
                    // move past do
                    current_input_index += 1;

                    break  'rule_check;
                },
                Rules::StmtToIf => 'rule_check: {
                    // if ( condition ) stmt elif
                    // make all terminals
                    let if_term = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::If), Value::Null);
                    let open = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Open), Value::Null);
                    let close = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Close), Value::Null);


                    // make all non terminals
                    let cond = TreeNode::child(current_rule_Node, Rules::ConditionMarker, Value::Null);
                    let stmt = TreeNode::child(current_rule_Node, Rules::StmtMarker, Value::Null);
                    let elif = TreeNode::child(current_rule_Node, Rules::ElifMarker, Value::Null);


                    // add nodes in order to parent
                    tree.put(current_rule_Node, if_term);
                    tree.put(current_rule_Node, open);
                    let cond_index = tree.put(current_rule_Node, cond);
                    tree.put(current_rule_Node, close);
                    let stmt_index = tree.put(current_rule_Node, stmt);
                    let elif_index = tree.put(current_rule_Node, elif);

                    // add elif then stmt then condition into the stack
                    rules_to_process.push_front((Rules::ElifMarker, elif_index));
                    rules_to_process.push_front((Rules::StmtMarker, stmt_index));
                    rules_to_process.push_front((Rules::Jump(1), stmt_index)); // consume close )
                    rules_to_process.push_front((Rules::ConditionMarker, cond_index));

                    // eat the if and the (
                    current_input_index += 2;
                    break  'rule_check;
                },

                Rules::DeclToVar =>  'rule_check:{
                    //basic arr var ;
                    // make all tokens
                    let basic_type:Value;
                    match &input[current_input_index] {
                        tokens::Basic(v) =>{
                            basic_type = v.clone();
                        }
                        _ => {
                            return Err(format!("no basic found"));
                        } 
                    }
                    let basic = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Basic(basic_type)), Value::Null);
                    let stop = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Stop), Value::Null);
                    tree.put(current_rule_Node, basic) ;

                    // see if there is any arry there
                    if let &tokens::ArrayEquationStart = &input[current_input_index+1] {
                        // there is an array here
                        // set up markers and deal with arr
                        let arr = TreeNode::child(current_rule_Node, Rules::ArrToArrayArr, Value::Null);
                        let var = TreeNode::child(current_rule_Node, Rules::VarToIDArr, Value::Null);

                        let arr_index = tree.put(current_rule_Node, arr);
                        let var_index = tree.put(current_rule_Node, var);
                        
                        // push in reverse order
                        rules_to_process.push_front((Rules::Jump(1), var_index));
                        rules_to_process.push_front((Rules::VarToIDArr, var_index));
                        rules_to_process.push_front((Rules::ArrToArrayArr, arr_index));
                    }
                    else {
                        // no array here :3
                        let arr = TreeNode::child(current_rule_Node, Rules::ArrToEpsilon, Value::Null);
                        let arr_index = tree.put(current_rule_Node, arr);
                        let epsilon = TreeNode::child(arr_index, Rules::Terminal(tokens::Null), Value::Null);
                        tree.put(arr_index, epsilon);

                        // send var to be delt with by something else lmao
                        let var = TreeNode::child(current_rule_Node, Rules::VarToIDArr, Value::Null);
                        let var_index = tree.put(current_rule_Node, var);
                        
                        // push to stack
                        rules_to_process.push_front((Rules::Jump(1), var_index));
                        rules_to_process.push_front((Rules::VarToIDArr, var_index));
                    }

                    // always end with pushing stop
                    current_input_index += 1; // move passed basic into arr or var
                    tree.put(current_rule_Node, stop);
                    break  'rule_check;
                },

                Rules::ArrMarker => 'rule_check: {
                    // detect type if there be an array here or not
                    if let &tokens::ArrayEquationStart = &input[current_input_index]{
                        // yep
                        tree.update_marker(current_rule_Node, Rules::ArrToArrayArr);
                        rules_to_process.push_front((Rules::ArrToArrayArr,current_rule_Node));
                    } else {
                        //nope
                        tree.update_marker(current_rule_Node, Rules::ArrToEpsilon);
                        rules_to_process.push_front((Rules::ArrToEpsilon,current_rule_Node));

                    }
                    break 'rule_check;
                }

                Rules::ArrToArrayArr => 'rule_check: {
                    // [ num ] arr
                    
                    // set up terminals
                    let arr_start = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::ArrayEquationStart), Value::Null);
                    let arr_end = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::ArrayEquationEnd), Value::Null);

                    // deal with condition
                    let cond_type = Self::detect_next_conditon_type(&input, current_input_index+1);
                    let cond = TreeNode::child(current_rule_Node, cond_type.clone(), Value::Null);

                    // deal with arr
                    let arr = TreeNode::child(current_rule_Node, Rules::ArrMarker, Value::Null);

                    // inject in correct order
                    tree.put(current_rule_Node, arr_start);
                    let cond_index = tree.put(current_rule_Node, cond);
                    tree.put(current_rule_Node, arr_end);
                    let arr_index = tree.put(current_rule_Node, arr);

                    // put rules into tree in reverse order
                    rules_to_process.push_front((Rules::ArrMarker, arr_index));
                    rules_to_process.push_front((Rules::Jump(1), arr_index));// move passed the ]
                    rules_to_process.push_front((cond_type, cond_index));
                    current_input_index += 1; // move passed the [
                    break 'rule_check;
                }

                Rules::VarToIDArr=> 'rule_check: {
                    // only one type of var 
                    // var -> id arr
                    
                    // take care of id
                    let id: TreeNode = TreeNode::child(current_rule_Node, Rules::Terminal(input[current_input_index].clone()), Value::Null);

                    let arr:TreeNode;
                    let arr_type:Rules;
                    // detect type of arr
                    if let &tokens::ArrayEquationStart = &input[current_input_index+1] {
                        // array
                        arr = TreeNode::child(current_rule_Node, Rules::ArrToArrayArr, Value::Null);
                        arr_type = Rules::ArrToArrayArr;
                    } else {
                        // not array
                        arr = TreeNode::child(current_rule_Node, Rules::ArrToEpsilon, Value::Null);
                        arr_type = Rules::ArrToEpsilon;
                    }
                    
                    // inject
                    tree.put(current_rule_Node, id);
                    let arr_index = tree.put(current_rule_Node, arr.clone());
                    rules_to_process.push_front((arr_type, arr_index));
                    
                    current_input_index += 1; // move passed id

                    break 'rule_check;
                }    

                Rules::ExprToAssignEquation => 'rule_check: {
                    // apply expr -> var = equation ;

                    // set up var
                    let var = TreeNode::child(current_rule_Node,Rules::VarToIDArr, Value::Null);
                    let var_index = tree.put(current_rule_Node, var);

                    // maker terminals
                    let equals = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Assigns), Value::Null); 
                    let stop = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Stop), Value::Null); 

                    //inject equals
                    tree.put(current_rule_Node, equals);

                    // detect and inject eq type 
                    let eq_node = TreeNode::child(current_rule_Node, Rules::EquationMarker, Value::Null); 
                    let eq_index =tree.put(current_rule_Node, eq_node);

                    // inject stop
                    tree.put(current_rule_Node, stop);


                    // push equation
                    rules_to_process.push_front((Rules::Jump(1), eq_index)); // jump pased ;
                    rules_to_process.push_front((Rules::EquationMarker, eq_index));
                    rules_to_process.push_front((Rules::Jump(1), eq_index)); // jump pased =
                    rules_to_process.push_front((Rules::VarToIDArr, var_index));

                    break 'rule_check;
                },

                Rules::EquationMarker => 'rule_check: { 
                    // detect type of equation and move on to that
                    let eq = Self::detect_next_equation_type(&input, current_input_index);
                    tree.update_marker(current_rule_Node, eq.clone());

                    rules_to_process.push_front((eq,current_rule_Node));

                    break 'rule_check;
                }

                Rules::EquationToNegate => 'rule_check: {
                    // detect type of equation and move on to that
                    let negation = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Minus), Value::Null);

                    let eq = Self::detect_next_equation_type(&input, current_input_index+1);
                    let next_eq = TreeNode::child(current_rule_Node, eq.clone(), Value::Null);
                    tree.put(current_rule_Node, negation);
                    let eq_index = tree.put(current_rule_Node, next_eq);

                    rules_to_process.push_front((eq, eq_index));
                    current_input_index +=1;

                    break 'rule_check;
                }

                Rules::EquationToEncloseHeq =>  'rule_check:{
                    // ( condition ) heq

                    //make all terminalblocks
                    let start_pren = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Open), Value::Null);
                    let end_pren = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Close), Value::Null);


                    
                    // detrermin condition type
                    let con_type = Self::detect_next_conditon_type(&input, current_input_index+1); //+1 added to get inside the ()
                    let cond = TreeNode::child(current_rule_Node, con_type.clone(), Value::Null);

                    let heq = TreeNode::child(current_rule_Node, Rules::HeqMarker, Value::Null);

                    //inject into parent
                    tree.put(current_rule_Node, start_pren);
                    let cond_index = tree.put(current_rule_Node, cond);
                    tree.put(current_rule_Node, end_pren);
                    let heq_index = tree.put(current_rule_Node, heq);

                    
                    // push next rules
                    rules_to_process.push_front((Rules::HeqMarker, heq_index));
                    rules_to_process.push_front((con_type, cond_index));
                    current_input_index += 1; // advance 1 token into the block
                    break 'rule_check;

                },

                Rules::EquationToObjectHeq =>  'rule_check: {
                    // object heq
                    let obj_type = Self::detect_next_object_type(&input, current_input_index);
                    let object = TreeNode::child(current_rule_Node, obj_type.clone(), Value::Null);
                    let object_index = tree.put(current_rule_Node, object);
                    
                    let heq = TreeNode::child(current_rule_Node, Rules::HeqMarker, Value::Null);
                    let heq_index = tree.put(current_rule_Node, heq);
                    
                    rules_to_process.push_front((Rules::HeqMarker, heq_index));
                    rules_to_process.push_front((obj_type,object_index));

                    break  'rule_check;
                },

                Rules::HeqMarker =>  'rule_check: {
                    //heq found! is there anything here?
                    let heq_type = Self::detect_heq_type(&input, current_input_index);
                    if let Rules::HeqToEpsilon = heq_type {
                        // check hcon
                        let hcon_type = Self::detect_hcon_type(&input, current_input_index);
                        if let Rules::HconToEpsilon = hcon_type {
                            // there really isnt anything here
                            rules_to_process.push_front((heq_type.clone(), current_rule_Node));
                            tree.update_marker(current_rule_Node, heq_type);
                            break  'rule_check;
                        }
                        else {
                            // its an hcon actually
                            rules_to_process.push_front((Rules::HeqToHcon, current_rule_Node));
                            tree.update_marker(current_rule_Node, Rules::HeqToHcon);
                            break  'rule_check;

                        }
                    } else {
                        rules_to_process.push_front((heq_type.clone(), current_rule_Node));
                        tree.update_marker(current_rule_Node, heq_type);
                        break  'rule_check;
                    }
                }
                
                Rules::HeqToHcon => 'rule_check: {
                    let hcon = TreeNode::child(current_rule_Node, Rules::HconMarker, Value::Null);
                    let hcon_ind = tree.put(current_rule_Node, hcon);
                    rules_to_process.push_front((Rules::HconMarker, hcon_ind));
                    break 'rule_check;
                }
                // deal with all heq's Theres only like 1 diffrence and thats the begining lol
                Rules::HeqToDevide | Rules::HeqToSubtract | Rules::HeqToMultiply | Rules::HeqToAdd => 'rule_check: {
                    // get opperayor
                    let opperator:TreeNode;
                    match current_rule {
                        Rules::HeqToAdd => {
                            opperator = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Plus), Value::Null); 
                        }
                        Rules::HeqToSubtract => {
                            opperator = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Minus), Value::Null); 
                        }
                        Rules::HeqToMultiply => {
                            opperator = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Times), Value::Null); 
                        }
                        Rules::HeqToDevide => {
                            opperator = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Devides), Value::Null); 
                        }
                        _ =>{
                            // cant be nothing bc you have to go through marker to get here
                            panic!()
                        } 
                    }

                    // connect opperator
                    tree.put(current_rule_Node, opperator);
                    
                    // find next type 
                    let next_type = Self::detect_next_equation_type(&input, current_input_index+1);
                    let next_equation = TreeNode::child(current_rule_Node, next_type.clone(), Value::Null);
                    let equation_id = tree.put(current_rule_Node, next_equation);

                    rules_to_process.push_front((next_type, equation_id)); 
                    

                    // accorunt for opperator and enter the equation
                    current_input_index += 1;
                    break 'rule_check;
                }

                Rules::ConditionMarker => 'rule_check:{
                    //catch type of condition and then exicute it.
                    let next_cond = Self::detect_next_conditon_type(&input, current_input_index);
                    tree.update_marker(current_rule_Node, next_cond.clone());
                    rules_to_process.push_front((next_cond,current_rule_Node)); // go do it :3
                    break 'rule_check;
                },

                Rules::ConditionToNegate => 'rule_check: {
                    // detect type of equation and move on to that
                    let not = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Not), Value::Null);

                    let cond = Self::detect_next_conditon_type(&input, current_input_index+1);
                    let next_cond = TreeNode::child(current_rule_Node, cond.clone(), Value::Null);
                    tree.put(current_rule_Node, not);
                    let cond_index = tree.put(current_rule_Node, next_cond);
                    
                    rules_to_process.push_front((cond, cond_index));
                    current_input_index +=1;

                    break 'rule_check;
                }

                Rules::ConditionToEnclosedHcon => 'rule_check: {
                    // ( condition ) hcon

                    //make all terminalblocks
                    let start_pren = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Open), Value::Null);
                    let end_pren = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Close), Value::Null);


                    
                    // detrermin condition type
                    let con_type = Self::detect_next_conditon_type(&input, current_input_index+1); //+1 added to get inside the ()
                    let cond = TreeNode::child(current_rule_Node, con_type.clone(), Value::Null);

                    let hcon = TreeNode::child(current_rule_Node, Rules::HconMarker, Value::Null);

                    //inject into parent
                    tree.put(current_rule_Node, start_pren);
                    let cond_index = tree.put(current_rule_Node, cond);
                    tree.put(current_rule_Node, end_pren);
                    let hcon_index = tree.put(current_rule_Node, hcon);

                    
                    // push next rules
                    rules_to_process.push_front((Rules::HconMarker, hcon_index));
                    rules_to_process.push_front((Rules::Jump(1), hcon_index));
                    rules_to_process.push_front((con_type, cond_index));
                    current_input_index += 1; // advance 1 token into the block
                    break 'rule_check;
                },

                Rules::ConditionToObjectHcon => 'rule_check: {
                    // end of the condition
                    // object hcon
                    
                    let obj_type = Self::detect_next_object_type(&input, current_input_index);
                    let object = TreeNode::child(current_rule_Node, obj_type.clone(), Value::Null);
                    let object_index = tree.put(current_rule_Node, object);
                    
                    let hcon = TreeNode::child(current_rule_Node, Rules::HconMarker, Value::Null);
                    let hcon_index = tree.put(current_rule_Node, hcon);
                    
                    rules_to_process.push_front((Rules::HconMarker, hcon_index));
                    rules_to_process.push_front((obj_type,object_index));
                    break 'rule_check;
                },

                // everything objects
                Rules::ObjectToVar => {
                    // object-> var
                    // object as parent
                    let var = TreeNode::child(current_rule_Node, Rules::VarToIDArr, Value::Null); 
                    let var_index = tree.put(current_rule_Node, var);
                    rules_to_process.push_front((Rules::VarToIDArr,var_index));

                }
                
                Rules::ObjectToNum | Rules::ObjectToReal | Rules::ObjectToTrue |Rules::ObjectToFalse => {
                    let obj_type = current_rule.clone();


                    let object = TreeNode::child(current_rule_Node, obj_type, Value::Null);
                    let object_index = tree.put(current_rule_Node, object);

                    let terminal = TreeNode::child(object_index, Rules::Terminal(input[current_input_index].clone()), Value::Null); 
                    tree.put(object_index, terminal);
                    current_input_index +=1; // move passed terminal
                }

                Rules::HconMarker => 'rule_check: {
                    //Hcon found! is there anything here?
                    let hcon_type = Self::detect_hcon_type(&input, current_input_index);
                    if let Rules::HconToEpsilon = hcon_type {
                        // check heq
                        let heq_type = Self::detect_heq_type(&input, current_input_index);
                        if let Rules::HeqToEpsilon = heq_type {
                            // there really isnt anything here
                            rules_to_process.push_front((hcon_type.clone(), current_rule_Node));
                            tree.update_marker(current_rule_Node, hcon_type);
                            break  'rule_check;
                        }
                        else {
                            // its an heq actually
                            rules_to_process.push_front((Rules::HconToHeq, current_rule_Node));
                            tree.update_marker(current_rule_Node, Rules::HconToHeq);
                            break  'rule_check;

                        }
                    } else {
                        rules_to_process.push_front((hcon_type.clone(), current_rule_Node));
                        tree.update_marker(current_rule_Node, hcon_type);
                        break  'rule_check;
                    }
                }

                Rules::HconToHeq => 'rule_check: {
                    let heq = TreeNode::child(current_rule_Node, Rules::HeqMarker, Value::Null);
                    let heq_ind = tree.put(current_rule_Node, heq);
                    rules_to_process.push_front((Rules::HeqMarker, heq_ind));
                    break 'rule_check;
                }
                
                // deal with all of hcon
                Rules::HconToEquals | Rules::HconToNoteq | Rules::HconToLess | Rules::HconToLessEq | Rules::HconToMore | Rules::HconToMoreEq | Rules::HconToAnd | Rules::HconToOr => 'rule_check:{
                    // get opperator
                    let opperator:TreeNode;
                    match current_rule {
                        Rules::HconToEquals => {
                            opperator = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Equals), Value::Null); 
                        }
                        Rules::HconToNoteq => {
                            opperator = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Equals), Value::Null); 
                        }
                        Rules::HconToLess => {
                            opperator = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Les), Value::Null); 
                        }
                        Rules::HconToLessEq => {
                            opperator = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Leq), Value::Null); 
                        }
                        Rules::HconToMore => {
                            opperator = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Gre), Value::Null); 
                        }
                        Rules::HconToMoreEq => {
                            opperator = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Geq), Value::Null); 
                        }
                        Rules::HconToAnd => {
                            opperator = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::And), Value::Null); 
                        }
                        Rules::HconToOr => {
                            opperator = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Or), Value::Null); 
                        }
                        _ =>{
                            panic!()
                        }
                    }

                    // connect opperator
                    tree.put(current_rule_Node, opperator);
                    
                    // find next type 
                    let next_type = Self::detect_next_conditon_type(&input, current_input_index+1); // start detecting after current opperator
                    let next_equation = TreeNode::child(current_rule_Node, next_type.clone(), Value::Null);
                    let equation_id = tree.put(current_rule_Node, next_equation);

                    rules_to_process.push_front((next_type, equation_id));
                    

                    // account for opperator
                    current_input_index = current_input_index + 1;
                    break 'rule_check;
                }

                // elif
                Rules::ElifMarker => 'rule_check: {
                    // 1 determine if there is an else
                    if input[current_input_index] == tokens::Else {
                        // well. Theres an else it seams
                        rules_to_process.push_front((Rules::ElifToElse,current_rule_Node)); // go do it :3
                        tree.update_marker(current_rule_Node, Rules::ElifToElse);
                    } else {
                        // guesss its nothing
                        rules_to_process.push_front((Rules::ElifToEpsilon,current_rule_Node));
                        tree.update_marker(current_rule_Node, Rules::ElifToEpsilon);
                    }
                    break 'rule_check;
                },

                Rules::ElifToElse => 'rule_check: {
                    // else stmt
                    
                    // add in terminal else and then go to stmt block
                    let els = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Else), Value::Null); 
                    tree.put(current_rule_Node, els);

                    // stmt
                    rules_to_process.push_front((Rules::StmtMarker, current_rule_Node));

                    current_input_index += 1; // advance 1 token
                    break 'rule_check;
                },

                // deal with epsilon
                Rules::BlockToEpsilon  => 'rule_check:{
                    // add in the terminal epsilon
                    // only happens when encountered } in block to statement block
                    // dont do anything just go past
                    let epsilon = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Null), Value::Null); 
                    tree.put(current_rule_Node, epsilon);

                    break 'rule_check;
                },

                Rules::ElifToEpsilon => 'rule_check: {
                    // add in the terminal epsilon
                    // only happens when if statement has ended. no clue what could be next so dont do anything
                    // dont do anything just go past
                    let epsilon = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Null), Value::Null); 
                    tree.put(current_rule_Node, epsilon);

                    // do not advance any tokens
                    break 'rule_check;
                },

                Rules::HeqToEpsilon | Rules::HconToEpsilon => 'rule_check: {
                    // nothings here and dont do anything catch put in epsilon and move on
                    let epsilon = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Null), Value::Null); 
                    tree.put(current_rule_Node, epsilon);

                    break 'rule_check;
                }

                Rules::ArrToEpsilon => 'rule_check: {
                    // nothings here and dont do anything catch put in epsilon and move on
                    let epsilon = TreeNode::child(current_rule_Node, Rules::Terminal(tokens::Null), Value::Null); 
                    tree.put(current_rule_Node, epsilon);

                    break 'rule_check;
                }

                Rules::Jump(n) => 'rule_check: {
                    // dev only
                    let mut skiped_tokens: Vec<tokens> = vec![];
                    if n+current_input_index < input.len() {
                        for t in &input[current_input_index..current_input_index+n]{
                            skiped_tokens.push(t.clone());
                        }
                    }
                    

                    println!("Skipping the following tokens {:?}", skiped_tokens);
                    current_input_index += n;
                    break 'rule_check;
                }

                _ => {
                    println!("Encountered rule not accounted for. {:?} with rule list {:?} at index {}", current_rule, rules_to_process, current_input_index);
                    return Err(format!("Failed to parse :( {:?} with rule list {:?} at index {}", current_rule, rules_to_process, current_input_index))
                }
            }
        
               
            // to to next rule in the heep
            if !rules_to_process.is_empty() {
                let next= rules_to_process.pop_front().unwrap();
                current_rule = next.0;
                current_rule_Node = next.1;

                let current_rule_parent = tree.get(current_rule_Node).unwrap().parent.unwrap();
                //let current_rule_parent = 0;
                // print the current command for debug purposes
                println!("Currently executing rule {:?} whos parent rule is {:?} at index {}", current_rule.render(), tree.get(current_rule_parent).unwrap().rule.render() ,current_input_index);
                // normal print command
                //println!("{:?}", current_rule.render());
                
            }
            else if current_input_index < input.len() {
                // ran out of rules and not tokens
                println!("Ran out of rules. {:?} with rule list {:?} at index {}", current_rule, rules_to_process, current_input_index);
                return Err(format!("no more rules at index: {} and token: {:?}",current_input_index,input[current_input_index]));
            }
        }
        return Ok(tree);
    }

    fn detect_next_equation_type(input: &Vec<tokens>, offset:usize) -> Rules{
        if let tokens::Open = input[offset] {
            return Rules::EquationToEncloseHeq;
        } else if let tokens::Minus = input[offset]  {
            return Rules::EquationToNegate;
        }
        return Rules::EquationToObjectHeq;
    }

    fn detect_next_object_type(input: &Vec<tokens>, offset:usize) -> Rules{
        match &input[offset] {
                tokens::Id(_) =>{ return Rules::ObjectToVar}
                tokens::Number(_) => {return Rules::ObjectToNum}
                tokens::Real(_) => {return Rules::ObjectToReal}
                tokens::True => {return Rules::ObjectToTrue}
                tokens::False => {return Rules::ObjectToFalse}
                object => {
                    panic!("Invalid Object Type: {:?}", object);
                }
            }
    }

    fn detect_heq_type(input: &Vec<tokens>,offset:usize) -> Rules {
        match input[offset]{
            tokens::Plus => {
                return Rules::HeqToAdd;
            }
            tokens::Minus => {
                return Rules::HeqToSubtract;
            }
            tokens::Times => {
                return Rules::HeqToMultiply;
            }
            tokens::Devides => {
                return Rules::HeqToDevide;
            }
            _ => {
                // nothing found. Epsilon and leave
                return Rules::HeqToEpsilon;
            }
        }
    }

    fn detect_hcon_type(input: &Vec<tokens>,offset:usize) -> Rules {
        match input[offset] {
            tokens::Equals =>{
                return Rules::HconToEquals;
            }
            tokens::Neq =>{
                return Rules::HconToNoteq;
            }
            tokens::Les =>{
                return Rules::HconToLess;
            }
            tokens::Leq =>{
                return Rules::HconToLessEq;
            }
            tokens::Gre =>{
                return Rules::HconToMore;
            }
            tokens::Geq =>{
                return Rules::HconToMoreEq;
            }
            tokens::And =>{
                return Rules::HconToAnd;
            }
            tokens::Or =>{
                return Rules::HconToOr;
            }
            _ => {
                return Rules::HconToEpsilon;
            }
        }
    }

    fn is_end_token(t: &tokens) -> bool {
        match t {
            tokens::ArrayEquationEnd | tokens::Close | tokens::Stop | tokens::EndEnclose => {
                return true;
            }
            _ => {return false;}
        }
    }

    fn detect_id_array(input: &Vec<tokens>, offset:usize) -> bool{
        match input[offset+1] {
            tokens::ArrayEquationStart => {
                return true;
            }
            _ => {
                return false;
            }
        }
    }

    fn detect_next_conditon_type(input: &Vec<tokens>, offset:usize) -> Rules{
        if let tokens::Open = input[offset] {
            return Rules::ConditionToEnclosedHcon;
        } else if let tokens::Not = input[offset]  {
            return Rules::ConditionToNegate;
        }
        return Rules::ConditionToObjectHcon;
    }
}





use super::lexical_analyzer::{Lexer};


#[test]
pub fn test_parser() {
    let code = "{int i;\n\twhile (true)\n\t\ti = i + 1;}";
    let mut par = Lexer{root_env:Env::new(None)};
    let result = par.custom_lexer(code).unwrap();
    println!("{}", Lexer::renderer(result.clone()));
    par.root_env.detailed_print_all();
    
    
    let res = Parser::parse(result, par.root_env);

    if res.is_err(){
        println!("Error: {}",res.unwrap_err())
    } else {
        println!("Parsing Finished");
        //println!("{:?}",res.unwrap());
    }
}

/* 3
StartEnclose, Basic(AnyInt), Id("x"), Stop, Basic(AnyInt), 

Id("y"), Stop, Id("x"), Assigns, Number(1), 

10
Stop, Id("y"), Assigns, Number(5), Stop, 

While, Open, Id("x"), Leq, Number(5), 

20
And, Id("y"), Neq, Number(0), Or, 

Not, Open, Id("y"), Les, Id("x"), 

30
Close, Close, StartEnclose, Id("x"), Assigns, 

Id("x"), Plus, Number(1), Stop, Id("y"), 

40
Assigns, Id("y"), Minus, Number(1), Stop, 

EndEnclose, EndEnclose

 */