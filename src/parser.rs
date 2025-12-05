//parser.rs

use std::cell::RefCell;
use std::collections::{VecDeque};
use std::rc::Rc;

use super::lexical_analyzer::{tokens};
use super::symbol_table::{Env, Value};

#[derive(Debug,Clone)]
pub struct TreeNode {
    pub parent: Option<usize>,
    pub this_node: Option<usize>,
    pub children: Vec<usize>,
    pub rule:Rules,
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

#[derive(Debug,Clone)]
pub struct TreeControler {
    pub tree_vector: Vec<TreeNode>
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

    pub fn get_weak(&self, node_index:usize) -> Option<&TreeNode>{
        return self.tree_vector.get(node_index);
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

    pub fn render_node(&self, node_index:usize, depth:usize){
        let indent = "  |".repeat(depth);
        // render self
        println!("{}{}",indent,self.tree_vector[node_index].rule.clone().render());

        // render children
        for child in self.tree_vector[node_index].clone().children{
            Self::render_node(self, child,depth+1);
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
pub enum Rules{
    // Parser commands
    Jump(usize), // jumps counter n spaces
    ExitEnviornment,
    EnterEnviornment,

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
    EquationToNegCondition,

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
    ConditionToNegEquation,


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

            Rules::EquationToEncloseHeq => { return "equation -> ( equation ) heq".to_string()},
            Rules::EquationToObjectHeq => { return "equation -> object heq".to_string()},
            Rules::EquationToNegate => { return "equation -> - equation".to_string()},
            Rules::EquationToNegCondition => { return "equation -> ! condition".to_string()},

            Rules::ObjectToVar => { return "object -> var".to_string()},
            Rules::ObjectToNum => { return "object -> num".to_string()},
            Rules::ObjectToReal => { return "object -> real".to_string()},
            Rules::ObjectToTrue => { return "object -> true".to_string()},
            Rules::ObjectToFalse => { return "object -> false".to_string()},

            Rules::ConditionToEnclosedHcon => { return "condition -> ( condition ) hcon".to_string()},
            Rules::ConditionToObjectHcon => { return "condition -> object hcon".to_string()},
            Rules::ConditionToNegate => { return "condition -> ! condition".to_string()},
            Rules::ConditionToNegEquation => { return "condition -> - equation".to_string()},

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

            Rules::Terminal(t) => { return format!("Terminal: {}",t.clone().render())},
            Rules::Jump(d) => { return format!("Skip {} spaces", d )},
            Rules::EnterEnviornment => { return "Entered Enviornment".to_string()},
            Rules::ExitEnviornment => { return "Exited Enviornment".to_string()},

            r => {return format!("Not Implemented {:?}", r);}
        }
    }
}

struct EnvControler {
    pub root_env: Rc<Env>,
    prev_env:Option<Rc<EnvControler>>,
    env_count:RefCell<usize>,
}

impl EnvControler {
    pub fn new_root(root_env: Rc<Env>) -> Rc<EnvControler> {
        EnvControler {
            root_env: root_env,
            env_count: RefCell::new(0),
            prev_env: None,
        }.into()
    }
    fn next_env(this:&mut Rc<EnvControler>) -> Rc<EnvControler> {
        let borrowed_list = this.root_env.children.borrow();
        let mut current_index = this.env_count.borrow_mut();

        if *current_index < borrowed_list.len().into(){
            let next_child = &this.root_env.children.borrow()[*current_index];

            *current_index += 1;

            let next_root = next_child.upgrade().unwrap();
            let new_controler = Self {
                root_env: Rc::clone(&next_root),
                prev_env: Some(Rc::clone(&this)),
                env_count: RefCell::new(0),
            };
            return Rc::new(new_controler);
        } else {
            panic!("No Next Environment")
        }
    } 
    fn prev_env(this:&mut Rc<EnvControler>) -> Rc<EnvControler> {
        if let Some(parent) = &this.prev_env {
            return Rc::clone(parent);
        }
        else {
            panic!("No Prev Environment")
        }
    }
}

pub struct Parser{
    pub all_scopes: Vec<Rc<Env>>,
}

impl Parser {
    pub fn parse(&self,input: Vec<tokens>, root_env: &Rc<Env>) -> Result<TreeControler, String>{
        let mut current_input_index:usize = 0;
        let mut tree = TreeControler::new();
        let mut current_rule = Rules::ProgramToBlock;
        let mut current_rule_node:usize = 0;
        let mut rules_to_process: VecDeque<(Rules, usize)> = VecDeque::new(); // acting as a heap
        let mut current_enviornment = EnvControler::new_root(root_env.clone());


        println!("Parsing started.");
        //println!("{}", current_rule.render());

        while current_input_index <= input.len(){

            match current_rule  {

                // deal with blocks
                Rules::ProgramToBlock | Rules::StmtToBlock =>  'rule_check:{
                    //rule only encountered in 2 sinarios. Start of program. or block that looks like this { block }
                    // either way it makes a block that looks like thiis { block } so only deal with that and error otherwise
                    
                    // manage tree
                    
                    
                    //make all blocks
                    let start_enclose = TreeNode::child(current_rule_node, Rules::Terminal(tokens::StartEnclose), Value::Null);
                    let stmt_block = TreeNode::child(current_rule_node, Rules::BlockToStmtBlock, Value::Null); 
                    let end_enclose = TreeNode::child(current_rule_node, Rules::Terminal(tokens::EndEnclose), Value::Null);

                    //inject into parent
                    tree.put(current_rule_node, start_enclose);
                    let stmt_index = tree.put(current_rule_node, stmt_block);
                    tree.put(current_rule_node, end_enclose);

                    // push next rule
                    rules_to_process.push_front((Rules::ExitEnviornment, stmt_index));
                    rules_to_process.push_front((Rules::BlockToStmtBlock, stmt_index));
                    rules_to_process.push_front((Rules::EnterEnviornment, stmt_index));
                    current_input_index += 1; // advance 1 token
                    break  'rule_check;
                },

                // specal combination. Statement block deals withe verything block
                Rules::BlockToStmtBlock  => 'outer: {
                    // all this rule is saying is that we are allowing 2 statements in a row.
                    // logic: use this to deal with a statement (something ending with ';')
                    // logic: detect } symbol to end block and return epsilon


                    // recurse. placed first so its exicuted after whatever is inside the statement.
                    rules_to_process.push_front((Rules::BlockToStmtBlock, current_rule_node));

                    match &input[current_input_index] {
                        tokens::EndEnclose => {
                            // remove the recursion
                            rules_to_process.pop_front();

                            // epsilon
                            rules_to_process.push_front((Rules::BlockToEpsilon, current_rule_node));
                            current_input_index += 1; // advance 1 token
                            break 'outer; // break to outer match
                        }

                        // detect all types of statements
                        tokens::StartEnclose => 'inner: {
                            // { block }
                            let stmt_block = TreeNode::child(current_rule_node, Rules::StmtToBlock, Value::Null);
                            let stmt_index = tree.put(current_rule_node, stmt_block);

                            rules_to_process.push_front((Rules::StmtToBlock, stmt_index));
                            break 'inner;
                        }

                        tokens::Basic(v) => 'inner: {
                            // decl
                            let decl_block = TreeNode::child(current_rule_node, Rules::StmtToDecl, v.clone());
                            let decl_index = tree.put(current_rule_node, decl_block);
                            
                            rules_to_process.push_front((Rules::StmtToDecl, decl_index));
                            break 'inner;
                        }

                        tokens::Break => 'inner: {
                            // break ;
                            let break_block = TreeNode::child(current_rule_node, Rules::StmtToBreak,  Value::Null);
                            let break_index = tree.put(current_rule_node, break_block);
                            
                            rules_to_process.push_front((Rules::StmtToBreak, break_index));
                            break 'inner;
                        }

                        tokens::While => 'inner: {
                            // while ( condition ) { block }
                            let block = TreeNode::child(current_rule_node, Rules::StmtToWhile,  Value::Null);
                            let index = tree.put(current_rule_node, block);
                            
                            rules_to_process.push_front((Rules::StmtToWhile, index));
                            break 'inner;
                        }

                        tokens::Do => 'inner: {
                            // do stmt while ( condition ) ;
                            let block = TreeNode::child(current_rule_node, Rules::StmtToDo, Value::Null);
                            let index = tree.put(current_rule_node, block);
                            
                            rules_to_process.push_front((Rules::StmtToDo, index));
                            break 'inner;
                        }

                        tokens::If => 'inner: {
                            // if ( condition ) stmt elif
                            let block = TreeNode::child(current_rule_node, Rules::StmtToIf, Value::Null);
                            let index = tree.put(current_rule_node, block);
                            
                            rules_to_process.push_front((Rules::StmtToIf, index));
                            break 'inner;
                        }

                        tokens::Id(v) => 'inner: {
                            // expr
                            let value = Env::get(&current_enviornment.root_env,v.to_string());
                            let block: TreeNode;
                            if value.is_some(){
                                block = TreeNode::child(current_rule_node, Rules::StmtToExpr,value.unwrap());
                            } else {
                                println!("tree dump");
                                Env::print_detailed_down(&current_enviornment.root_env, 0);
                                return Err(format!("Id fetch failure for id {}",v));
                            }
                            let index = tree.put(current_rule_node, block);
                            
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

                            tree.update_marker(current_rule_node, Rules::StmtToBlock);
                            rules_to_process.push_front((Rules::StmtToBlock, current_rule_node));
                            break 'inner;
                        }

                        tokens::Basic(v) => 'inner: {
                            // decl
                            
                            tree.update_marker(current_rule_node, Rules::StmtToDecl);
                            rules_to_process.push_front((Rules::StmtToDecl, current_rule_node));
                            break 'inner;
                        }

                        tokens::Break => 'inner: {
                            // break ;
                            
                            tree.update_marker(current_rule_node, Rules::StmtToBreak);
                            rules_to_process.push_front((Rules::StmtToBreak, current_rule_node));
                            break 'inner;
                        }

                        tokens::While => 'inner: {
                            // while ( condition ) { block }
                            
                            tree.update_marker(current_rule_node, Rules::StmtToWhile);
                            rules_to_process.push_front((Rules::StmtToWhile, current_rule_node));
                            break 'inner;
                        }

                        tokens::Do => 'inner: {
                            // do stmt while ( condition ) ;
                            
                            tree.update_marker(current_rule_node, Rules::StmtToDo);
                            rules_to_process.push_front((Rules::StmtToDo, current_rule_node));
                            break 'inner;
                        }

                        tokens::If => 'inner: {
                            // if ( condition ) stmt elif
                            
                            tree.update_marker(current_rule_node, Rules::StmtToIf);
                            rules_to_process.push_front((Rules::StmtToIf, current_rule_node));
                            break 'inner;
                        }

                        tokens::Id(v) => 'inner: {
                            // expr
                            let value = Env::get(&current_enviornment.root_env,v.to_string());
                            let block = TreeNode::child(current_rule_node, Rules::StmtToExpr,value.unwrap());
                            let index = tree.put(current_rule_node, block);
                            
                            tree.update_marker(current_rule_node, Rules::StmtToExpr);
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
                    let basic = TreeNode::child(current_rule_node, Rules::Terminal(input[current_input_index].clone()), Value::Null);
                    let var = TreeNode::child(current_rule_node, Rules::DeclToVar, Value::Null); 
                    let stop = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Stop), Value::Null);

                    //inject into parent
                    tree.put(current_rule_node, basic);
                    let var_index = tree.put(current_rule_node, var);
                    tree.put(current_rule_node, stop);


                    // push next rule
                    rules_to_process.push_front((Rules::DeclToVar, var_index));
                    break  'rule_check;
                },

                Rules::StmtToExpr =>  'rule_check: {
                    // apply stmt -> expr
                    
                    // make blocks
                    let expr = TreeNode::child(current_rule_node, Rules::ExprToAssignEquation, Value::Null); 

                    //inject into parent
                    let expr_index = tree.put(current_rule_node, expr);


                    // push next rule

                    rules_to_process.push_front((Rules::ExprToAssignEquation, expr_index));
                    // do not consume anything
                    break  'rule_check;
                },
                Rules::StmtToBreak =>  'rule_check: {
                    // make blocks
                    let basic = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Break), Value::Null);
                    let stop = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Stop), Value::Null);

                    //inject into parent
                    tree.put(current_rule_node, basic);
                    tree.put(current_rule_node, stop);


                    // push next rule
                    current_input_index += 2; // advance passed stop
                    break  'rule_check;
                },
                Rules::StmtToWhile =>  'rule_check: {
                    // while ( condition ) stmt
                    // 2 recurses here. Recurse block first then condition so block is deeper in the heep
                    
                    // specify this one because it goes last and is more general

                    // nonterminals
                    let whil = TreeNode::child(current_rule_node, Rules::Terminal(tokens::While), Value::Null);
                    let open = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Open), Value::Null);
                    let close = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Close), Value::Null);

                    // condition
                    let next_cond = Self::detect_next_conditon_type(&input,current_input_index + 2);
                    let cond = TreeNode::child(current_rule_node, next_cond.clone(), Value::Null);

                    // statement
                    let stmt = TreeNode::child(current_rule_node, Rules::StmtMarker, Value::Null);


                    // add to tree
                    tree.put(current_rule_node, whil);
                    tree.put(current_rule_node, open);
                    let condition = tree.put(current_rule_node, cond);
                    tree.put(current_rule_node, close);
                    let statement = tree.put(current_rule_node, stmt);

                    // push next rule
                    rules_to_process.push_front((Rules::StmtMarker, statement));
                    rules_to_process.push_front((Rules::Jump(1), condition)); // consume close )
                    rules_to_process.push_front((next_cond, condition));
                    
                    // consume while and open
                    current_input_index += 2;
                    break  'rule_check;
                },
                Rules::StmtToDo =>  'rule_check: {
                    // do stmt while ( condition ) ;

                    // build terminals
                    let do_term = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Do), Value::Null);
                    let while_term = TreeNode::child(current_rule_node, Rules::Terminal(tokens::While), Value::Null);
                    let open = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Open), Value::Null);
                    let close = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Close), Value::Null);
                    let stop = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Stop), Value::Null);

                    // build stmt
                    let stmt = TreeNode::child(current_rule_node, Rules::StmtMarker, Value::Null);

                    // build condition
                    let cond = TreeNode::child(current_rule_node, Rules::ConditionMarker, Value::Null);


                    // add to parent in the right order
                    tree.put(current_rule_node, do_term);
                    let stmt_index = tree.put(current_rule_node, stmt);
                    tree.put(current_rule_node, while_term);
                    tree.put(current_rule_node, open);
                    let cond_index = tree.put(current_rule_node, cond);
                    tree.put(current_rule_node, close);
                    tree.put(current_rule_node, stop);


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
                    let if_term = TreeNode::child(current_rule_node, Rules::Terminal(tokens::If), Value::Null);
                    let open = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Open), Value::Null);
                    let close = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Close), Value::Null);


                    // make all non terminals
                    let cond = TreeNode::child(current_rule_node, Rules::ConditionMarker, Value::Null);
                    let stmt = TreeNode::child(current_rule_node, Rules::StmtMarker, Value::Null);
                    let elif = TreeNode::child(current_rule_node, Rules::ElifMarker, Value::Null);


                    // add nodes in order to parent
                    tree.put(current_rule_node, if_term);
                    tree.put(current_rule_node, open);
                    let cond_index = tree.put(current_rule_node, cond);
                    tree.put(current_rule_node, close);
                    let stmt_index = tree.put(current_rule_node, stmt);
                    let elif_index = tree.put(current_rule_node, elif);

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
                    let basic = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Basic(basic_type)), Value::Null);
                    let stop = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Stop), Value::Null);
                    tree.put(current_rule_node, basic) ;

                    // see if there is any arry there
                    if let &tokens::ArrayEquationStart = &input[current_input_index+1] {
                        // there is an array here
                        // set up markers and deal with arr
                        let arr = TreeNode::child(current_rule_node, Rules::ArrToArrayArr, Value::Null);
                        let var = TreeNode::child(current_rule_node, Rules::VarToIDArr, Value::Null);

                        let arr_index = tree.put(current_rule_node, arr);
                        let var_index = tree.put(current_rule_node, var);
                        
                        // push in reverse order
                        rules_to_process.push_front((Rules::Jump(1), var_index));
                        rules_to_process.push_front((Rules::VarToIDArr, var_index));
                        rules_to_process.push_front((Rules::ArrToArrayArr, arr_index));
                    }
                    else {
                        // no array here :3
                        let arr = TreeNode::child(current_rule_node, Rules::ArrToEpsilon, Value::Null);
                        let arr_index = tree.put(current_rule_node, arr);
                        let epsilon = TreeNode::child(arr_index, Rules::Terminal(tokens::Null), Value::Null);
                        tree.put(arr_index, epsilon);

                        // send var to be delt with by something else lmao
                        let var = TreeNode::child(current_rule_node, Rules::VarToIDArr, Value::Null);
                        let var_index = tree.put(current_rule_node, var);
                        
                        // push to stack
                        rules_to_process.push_front((Rules::Jump(1), var_index));
                        rules_to_process.push_front((Rules::VarToIDArr, var_index));
                    }

                    // always end with pushing stop
                    current_input_index += 1; // move passed basic into arr or var
                    tree.put(current_rule_node, stop);
                    break  'rule_check;
                },

                Rules::ArrMarker => 'rule_check: {
                    // detect type if there be an array here or not
                    if let &tokens::ArrayEquationStart = &input[current_input_index]{
                        // yep
                        tree.update_marker(current_rule_node, Rules::ArrToArrayArr);
                        rules_to_process.push_front((Rules::ArrToArrayArr,current_rule_node));
                    } else {
                        //nope
                        tree.update_marker(current_rule_node, Rules::ArrToEpsilon);
                        rules_to_process.push_front((Rules::ArrToEpsilon,current_rule_node));

                    }
                    break 'rule_check;
                }

                Rules::ArrToArrayArr => 'rule_check: {
                    // [ num ] arr
                    
                    // set up terminals
                    let arr_start = TreeNode::child(current_rule_node, Rules::Terminal(tokens::ArrayEquationStart), Value::Null);
                    let arr_end = TreeNode::child(current_rule_node, Rules::Terminal(tokens::ArrayEquationEnd), Value::Null);

                    // deal with condition
                    let cond_type = Self::detect_next_conditon_type(&input, current_input_index+1);
                    let cond = TreeNode::child(current_rule_node, cond_type.clone(), Value::Null);

                    // deal with arr
                    let arr = TreeNode::child(current_rule_node, Rules::ArrMarker, Value::Null);

                    // inject in correct order
                    tree.put(current_rule_node, arr_start);
                    let cond_index = tree.put(current_rule_node, cond);
                    tree.put(current_rule_node, arr_end);
                    let arr_index = tree.put(current_rule_node, arr);

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
                    let id: TreeNode = TreeNode::child(current_rule_node, Rules::Terminal(input[current_input_index].clone()), Value::Null);

                    let arr:TreeNode;
                    let arr_type:Rules;
                    // detect type of arr
                    if let &tokens::ArrayEquationStart = &input[current_input_index+1] {
                        // array
                        arr = TreeNode::child(current_rule_node, Rules::ArrToArrayArr, Value::Null);
                        arr_type = Rules::ArrToArrayArr;
                    } else {
                        // not array
                        arr = TreeNode::child(current_rule_node, Rules::ArrToEpsilon, Value::Null);
                        arr_type = Rules::ArrToEpsilon;
                    }
                    
                    // inject
                    tree.put(current_rule_node, id);
                    let arr_index = tree.put(current_rule_node, arr.clone());
                    rules_to_process.push_front((arr_type, arr_index));
                    
                    current_input_index += 1; // move passed id

                    break 'rule_check;
                }    

                Rules::ExprToAssignEquation => 'rule_check: {
                    // apply expr -> var = equation ;

                    // set up var
                    let var = TreeNode::child(current_rule_node,Rules::VarToIDArr, Value::Null);
                    let var_index = tree.put(current_rule_node, var);

                    // maker terminals
                    let equals = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Assigns), Value::Null); 
                    let stop = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Stop), Value::Null); 

                    //inject equals
                    tree.put(current_rule_node, equals);

                    // detect and inject eq type 
                    let eq_node = TreeNode::child(current_rule_node, Rules::EquationMarker, Value::Null); 
                    let eq_index =tree.put(current_rule_node, eq_node);

                    // inject stop
                    tree.put(current_rule_node, stop);


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
                    tree.update_marker(current_rule_node, eq.clone());

                    rules_to_process.push_front((eq,current_rule_node));

                    break 'rule_check;
                }

                Rules::EquationToNegate | Rules::ConditionToNegEquation => 'rule_check: {
                    // detect type of equation and move on to that
                    let negation = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Minus), Value::Null);

                    let eq = Self::detect_next_equation_type(&input, current_input_index+1);
                    let next_eq = TreeNode::child(current_rule_node, eq.clone(), Value::Null);
                    tree.put(current_rule_node, negation);
                    let eq_index = tree.put(current_rule_node, next_eq);

                    rules_to_process.push_front((eq, eq_index));
                    current_input_index +=1;

                    break 'rule_check;
                }

                Rules::EquationToEncloseHeq =>  'rule_check:{
                    // ( equation ) heq

                    //make all terminalblocks
                    let start_pren = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Open), Value::Null);
                    let end_pren = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Close), Value::Null);


                    
                    // detrermin equation type
                    let eq_type = Self::detect_next_equation_type(&input, current_input_index+1); //+1 added to get inside the ()
                    let eq = TreeNode::child(current_rule_node, eq_type.clone(), Value::Null);

                    let heq = TreeNode::child(current_rule_node, Rules::HeqMarker, Value::Null);

                    //inject into parent
                    tree.put(current_rule_node, start_pren);
                    let cond_index = tree.put(current_rule_node, eq);
                    tree.put(current_rule_node, end_pren);
                    let heq_index = tree.put(current_rule_node, heq);

                    
                    // push next rules
                    rules_to_process.push_front((Rules::HeqMarker, heq_index));
                    rules_to_process.push_front((Rules::Jump(1), heq_index));
                    rules_to_process.push_front((eq_type, cond_index));
                    current_input_index += 1; // advance 1 token into the block
                    break 'rule_check;

                },

                Rules::EquationToObjectHeq =>  'rule_check: {
                    // object heq
                    let obj_type = Self::detect_next_object_type(&input, current_input_index);
                    let object = TreeNode::child(current_rule_node, obj_type.clone(), Value::Null);
                    let object_index = tree.put(current_rule_node, object);
                    
                    let heq = TreeNode::child(current_rule_node, Rules::HeqMarker, Value::Null);
                    let heq_index = tree.put(current_rule_node, heq);
                    
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
                            rules_to_process.push_front((heq_type.clone(), current_rule_node));
                            tree.update_marker(current_rule_node, heq_type);
                            break  'rule_check;
                        }
                        else {
                            // its an hcon actually
                            rules_to_process.push_front((Rules::HeqToHcon, current_rule_node));
                            tree.update_marker(current_rule_node, Rules::HeqToHcon);
                            break  'rule_check;

                        }
                    } else {
                        rules_to_process.push_front((heq_type.clone(), current_rule_node));
                        tree.update_marker(current_rule_node, heq_type);
                        break  'rule_check;
                    }
                }
                
                Rules::HeqToHcon => 'rule_check: {
                    let hcon = TreeNode::child(current_rule_node, Rules::HconMarker, Value::Null);
                    let hcon_ind = tree.put(current_rule_node, hcon);
                    rules_to_process.push_front((Rules::HconMarker, hcon_ind));
                    break 'rule_check;
                }
                // deal with all heq's Theres only like 1 diffrence and thats the begining lol
                Rules::HeqToDevide | Rules::HeqToSubtract | Rules::HeqToMultiply | Rules::HeqToAdd => 'rule_check: {
                    // get opperayor
                    let opperator:TreeNode;
                    match current_rule {
                        Rules::HeqToAdd => {
                            opperator = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Plus), Value::Null); 
                        }
                        Rules::HeqToSubtract => {
                            opperator = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Minus), Value::Null); 
                        }
                        Rules::HeqToMultiply => {
                            opperator = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Times), Value::Null); 
                        }
                        Rules::HeqToDevide => {
                            opperator = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Devides), Value::Null); 
                        }
                        _ =>{
                            // cant be nothing bc you have to go through marker to get here
                            panic!()
                        } 
                    }

                    // connect opperator
                    tree.put(current_rule_node, opperator);
                    
                    // find next type 
                    let next_type = Self::detect_next_equation_type(&input, current_input_index+1);
                    let next_equation = TreeNode::child(current_rule_node, next_type.clone(), Value::Null);
                    let equation_id = tree.put(current_rule_node, next_equation);

                    rules_to_process.push_front((next_type, equation_id)); 
                    

                    // accorunt for opperator and enter the equation
                    current_input_index += 1;
                    break 'rule_check;
                }

                Rules::ConditionMarker => 'rule_check:{
                    //catch type of condition and then exicute it.
                    let next_cond = Self::detect_next_conditon_type(&input, current_input_index);
                    tree.update_marker(current_rule_node, next_cond.clone());
                    rules_to_process.push_front((next_cond,current_rule_node)); // go do it :3
                    break 'rule_check;
                },

                Rules::ConditionToNegate | Rules::EquationToNegCondition=> 'rule_check: {
                    // detect type of equation and move on to that
                    let not = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Not), Value::Null);

                    let cond = Self::detect_next_conditon_type(&input, current_input_index+1);
                    let next_cond = TreeNode::child(current_rule_node, cond.clone(), Value::Null);
                    tree.put(current_rule_node, not);
                    let cond_index = tree.put(current_rule_node, next_cond);
                    
                    rules_to_process.push_front((cond, cond_index));
                    current_input_index +=1;

                    break 'rule_check;
                }

                Rules::ConditionToEnclosedHcon => 'rule_check: {
                    // ( condition ) hcon

                    //make all terminalblocks
                    let start_pren = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Open), Value::Null);
                    let end_pren = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Close), Value::Null);


                    
                    // detrermin condition type
                    let con_type = Self::detect_next_conditon_type(&input, current_input_index+1); //+1 added to get inside the ()
                    let cond = TreeNode::child(current_rule_node, con_type.clone(), Value::Null);

                    let hcon = TreeNode::child(current_rule_node, Rules::HconMarker, Value::Null);

                    //inject into parent
                    tree.put(current_rule_node, start_pren);
                    let cond_index = tree.put(current_rule_node, cond);
                    tree.put(current_rule_node, end_pren);
                    let hcon_index = tree.put(current_rule_node, hcon);

                    
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
                    let object = TreeNode::child(current_rule_node, obj_type.clone(), Value::Null);
                    let object_index = tree.put(current_rule_node, object);
                    
                    let hcon = TreeNode::child(current_rule_node, Rules::HconMarker, Value::Null);
                    let hcon_index = tree.put(current_rule_node, hcon);
                    
                    rules_to_process.push_front((Rules::HconMarker, hcon_index));
                    rules_to_process.push_front((obj_type,object_index));
                    break 'rule_check;
                },

                // everything objects
                Rules::ObjectToVar => {
                    // object-> var
                    // object as parent
                    let var = TreeNode::child(current_rule_node, Rules::VarToIDArr, Value::Null); 
                    let var_index = tree.put(current_rule_node, var);
                    rules_to_process.push_front((Rules::VarToIDArr,var_index));

                }
                
                Rules::ObjectToNum | Rules::ObjectToReal | Rules::ObjectToTrue |Rules::ObjectToFalse => {
                    // object-> teminial

                    let terminal = TreeNode::child(current_rule_node, Rules::Terminal(input[current_input_index].clone()), Value::Null); 
                    tree.put(current_rule_node, terminal);
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
                            rules_to_process.push_front((hcon_type.clone(), current_rule_node));
                            tree.update_marker(current_rule_node, hcon_type);
                            break  'rule_check;
                        }
                        else {
                            // its an heq actually
                            rules_to_process.push_front((Rules::HconToHeq, current_rule_node));
                            tree.update_marker(current_rule_node, Rules::HconToHeq);
                            break  'rule_check;

                        }
                    } else {
                        rules_to_process.push_front((hcon_type.clone(), current_rule_node));
                        tree.update_marker(current_rule_node, hcon_type);
                        break  'rule_check;
                    }
                }

                Rules::HconToHeq => 'rule_check: {
                    let heq = TreeNode::child(current_rule_node, Rules::HeqMarker, Value::Null);
                    let heq_ind = tree.put(current_rule_node, heq);
                    rules_to_process.push_front((Rules::HeqMarker, heq_ind));
                    break 'rule_check;
                }
                
                // deal with all of hcon
                Rules::HconToEquals | Rules::HconToNoteq | Rules::HconToLess | Rules::HconToLessEq | Rules::HconToMore | Rules::HconToMoreEq | Rules::HconToAnd | Rules::HconToOr => 'rule_check:{
                    // get opperator
                    let opperator:TreeNode;
                    match current_rule {
                        Rules::HconToEquals => {
                            opperator = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Equals), Value::Null); 
                        }
                        Rules::HconToNoteq => {
                            opperator = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Equals), Value::Null); 
                        }
                        Rules::HconToLess => {
                            opperator = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Les), Value::Null); 
                        }
                        Rules::HconToLessEq => {
                            opperator = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Leq), Value::Null); 
                        }
                        Rules::HconToMore => {
                            opperator = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Gre), Value::Null); 
                        }
                        Rules::HconToMoreEq => {
                            opperator = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Geq), Value::Null); 
                        }
                        Rules::HconToAnd => {
                            opperator = TreeNode::child(current_rule_node, Rules::Terminal(tokens::And), Value::Null); 
                        }
                        Rules::HconToOr => {
                            opperator = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Or), Value::Null); 
                        }
                        _ =>{
                            panic!()
                        }
                    }

                    // connect opperator
                    tree.put(current_rule_node, opperator);
                    
                    // find next type 
                    let next_type = Self::detect_next_conditon_type(&input, current_input_index+1); // start detecting after current opperator
                    let next_equation = TreeNode::child(current_rule_node, next_type.clone(), Value::Null);
                    let equation_id = tree.put(current_rule_node, next_equation);

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
                        rules_to_process.push_front((Rules::ElifToElse,current_rule_node)); // go do it :3
                        tree.update_marker(current_rule_node, Rules::ElifToElse);
                    } else {
                        // guesss its nothing
                        rules_to_process.push_front((Rules::ElifToEpsilon,current_rule_node));
                        tree.update_marker(current_rule_node, Rules::ElifToEpsilon);
                    }
                    break 'rule_check;
                },

                Rules::ElifToElse => 'rule_check: {
                    // else stmt
                    
                    // add in terminal else and then go to stmt block
                    let els = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Else), Value::Null); 
                    tree.put(current_rule_node, els);

                    // stmt
                    rules_to_process.push_front((Rules::StmtMarker, current_rule_node));

                    current_input_index += 1; // advance 1 token
                    break 'rule_check;
                },

                // deal with epsilon
                Rules::BlockToEpsilon  => 'rule_check:{
                    // add in the terminal epsilon
                    // only happens when encountered } in block to statement block
                    // dont do anything just go past
                    let epsilon = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Null), Value::Null); 
                    tree.put(current_rule_node, epsilon);

                    break 'rule_check;
                },

                Rules::ElifToEpsilon => 'rule_check: {
                    // add in the terminal epsilon
                    // only happens when if statement has ended. no clue what could be next so dont do anything
                    // dont do anything just go past
                    let epsilon = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Null), Value::Null); 
                    tree.put(current_rule_node, epsilon);

                    // do not advance any tokens
                    break 'rule_check;
                },

                Rules::HeqToEpsilon | Rules::HconToEpsilon => 'rule_check: {
                    // nothings here and dont do anything catch put in epsilon and move on
                    let epsilon = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Null), Value::Null); 
                    tree.put(current_rule_node, epsilon);

                    break 'rule_check;
                }

                Rules::ArrToEpsilon => 'rule_check: {
                    // nothings here and dont do anything catch put in epsilon and move on
                    let epsilon = TreeNode::child(current_rule_node, Rules::Terminal(tokens::Null), Value::Null); 
                    tree.put(current_rule_node, epsilon);

                    break 'rule_check;
                }

                // parser rules
                Rules::Jump(n) => 'rule_check: {
                    // dev only
                    let mut skiped_tokens: Vec<tokens> = vec![];
                    if n+current_input_index < input.len() {
                        for t in &input[current_input_index..current_input_index+n]{
                            skiped_tokens.push(t.clone());
                        }
                    }
                    //println!("Skipping the following tokens {:?}", skiped_tokens);
                    current_input_index += n;
                    break 'rule_check;
                }

                Rules::EnterEnviornment => 'rule_check: {
                    current_enviornment = EnvControler::next_env(&mut current_enviornment);
                    break 'rule_check;
                }

                Rules::ExitEnviornment => 'rule_check: {
                    current_enviornment = EnvControler::prev_env(&mut current_enviornment);
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
                current_rule_node = next.1;

                //let current_rule_parent = tree.get(current_rule_node).unwrap().parent.unwrap();
                //let current_rule_parent = 0;
                // print the current command for debug purposes
                //println!("Currently executing rule {:?} whos parent rule is {:?} at index {}", current_rule.render(), tree.get(current_rule_parent).unwrap().rule.render() ,current_input_index);
                // normal print command
                //println!("{}", current_rule.render());
                
            }
            else if current_input_index < input.len() {
                // ran out of rules and not tokens
                println!("Ran out of rules. {:?} with rule list {:?} at index {}", current_rule, rules_to_process, current_input_index);
                return Err(format!("no more rules at index: {} and token: {:?}",current_input_index,input[current_input_index]));
            } else {
                break;
            }
        }
        return Ok(tree);
    }

    fn detect_next_equation_type(input: &Vec<tokens>, offset:usize) -> Rules{
        if let tokens::Open = input[offset] {
            return Rules::EquationToEncloseHeq;
        } else if let tokens::Minus = input[offset]  {
            return Rules::EquationToNegate;
        } else if let tokens::Not = input[offset] {
            return Rules::EquationToNegCondition;
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
        }else if let tokens::Minus = input[offset]  {
            return Rules::ConditionToNegEquation;
        }
        return Rules::ConditionToObjectHcon;
    }
}
