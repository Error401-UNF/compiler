
use std::ops::Deref;
use std::{mem, usize};
use std::rc::{Rc, Weak};
use std::cell::{RefCell, RefMut};

use crate::{lexical_analyzer::tokens, symbol_table::Value};

use super::parser::{TreeNode, TreeControler, Rules};


type Index = Rc<RefCell<usize>>;

#[derive(Debug)]
pub struct IndexBox{
    value:Index
}
impl IndexBox {
    pub fn new(v:usize)-> Self{
        Self { value: Rc::new(RefCell::new(v)) }
    }
    pub fn get(&self) -> usize {
        self.value.borrow().to_owned()
    }
    pub fn getindex(&self) -> Index {
        Rc::clone(&self.value)
    }
}
impl Clone for IndexBox {
    fn clone(&self) -> Self {
        Self { value: Rc::clone(&self.value)}
    }
}

#[derive(Debug)]
pub struct SyntaxTreeNode {
    pub parent:Option<IndexBox>,
    pub opperator:Opperator,
    pub expected_type:Value,
    pub child_nodes: (Option<IndexBox>,Option<IndexBox>),
}
impl SyntaxTreeNode {
    fn start(op:Opperator) -> Self{
        SyntaxTreeNode {
            parent: None,
            opperator: op,
            expected_type: Value::Null,
            child_nodes: (None,None),
        }
    }
    fn child(parent_index:IndexBox, op:Opperator,value:Value) -> Self{
        SyntaxTreeNode {
            parent: Some(parent_index),
            opperator: op,
            expected_type: value,
            child_nodes: (None,None),
        }
    }
    pub fn render_node(&self, tree_vector:&Vec<Rc<RefCell<SyntaxTreeNode>>>, depth:usize, right:bool){
        let indent:String;
        if right {
            indent = "  |".repeat(depth);
        } else {
            indent = "  |".repeat(depth) + "l: ";
        }
        // render self

        println!("{}{:?}",indent,self.opperator);

        if self.child_nodes.0.is_some(){
            let left = &tree_vector[self.child_nodes.0.clone().unwrap().get()].borrow();
            left.render_node(tree_vector, depth+1,false);
        }
        if self.child_nodes.1.is_some(){
            let right = &tree_vector[self.child_nodes.1.clone().unwrap().get()].borrow();
            right.render_node(tree_vector, depth+1,true);
        }
    }
}
impl Default for SyntaxTreeNode {
    fn default() -> Self {
        // Use a state similar to the start constructor
        Self::start(Opperator::Terminal(tokens::Null))
    }
}

#[derive(Debug,Clone)]
pub struct SyntaxTree {
    pub tree_vector: Rc<RefCell<Vec<Rc<RefCell<SyntaxTreeNode>>>>>,
    pub top_node: IndexBox
}
impl SyntaxTree {
    pub fn new(op:Opperator) -> Self{
        let top = Rc::new(RefCell::new(SyntaxTreeNode::start(op)));
        SyntaxTree{
            tree_vector: Rc::new(RefCell::new(vec![Rc::clone(&top)])),
            top_node: IndexBox::new(0)
        }
    }


    pub fn put_left(&self, parent_index: IndexBox, op:Opperator, value:Value) -> Result<Index,String>{
        let child_node = SyntaxTreeNode::child(parent_index.clone(), op, value);
        let mut tree = self.tree_vector.borrow_mut();

        let child_position = tree.len();
        if let Some(mut_parent) = tree.get_mut(parent_index.get()) { 
            let refc = IndexBox::new(child_position);
            mut_parent.borrow_mut().child_nodes.0 = Some(refc.clone());
            tree.push(Rc::new(RefCell::new(child_node)));
            return Ok(refc.getindex());
        } else {
            return Err(format!("No parent found"));
        }
    }

    pub fn put_right(&self, parent_index: IndexBox, op:Opperator, value:Value) -> Result<Index,String>{
        let child_node = SyntaxTreeNode::child(parent_index.clone(), op, value);
        let mut tree = self.tree_vector.borrow_mut();

        let child_position = tree.len();
        if let Some(mut_parent) = tree.get_mut(parent_index.get()) {
            let refc = IndexBox::new(child_position);
            mut_parent.borrow_mut().child_nodes.1 = Some(refc.clone());
            tree.push(Rc::new(RefCell::new(child_node)));
            return Ok(refc.getindex());
        }        
        return Err(format!("No parent found"));
    }

    pub fn merge_left(&self, child_tree: SyntaxTree) -> Result<IndexBox, String> {
        self.merge_child(child_tree, true)
    }

    /// Merges the root of a `child_tree` into the right child slot of the current tree's root (index 0).
    /// Returns the IndexBox pointing to the new root of the merged subtree.
    pub fn merge_right(&self, child_tree: SyntaxTree) -> Result<IndexBox, String> {
        self.merge_child(child_tree, false)
    }

    /// Helper to merge a child tree's vector into the current tree's vector and fix pointers.
    /// This function handles the complex index remapping required for a vector-based tree structure.
    fn merge_child(&self, child_tree: SyntaxTree, is_left: bool) -> Result<IndexBox, String> {
        
        // 1. Prepare to merge vectors and get the starting index (offset)
        let mut main_tree_vec = self.tree_vector.borrow_mut();
        
        // Take ownership of the child's vector contents (RefCell access is dropped after drain)
        let child_tree_vec = mem::take(&mut *child_tree.tree_vector.borrow_mut());
        
        if child_tree_vec.is_empty() {
             return Err("Child tree vector is empty.".to_string());
        }

        let offset = main_tree_vec.len();
        let child_root_new_index = offset;
        let child_root_node_ref = IndexBox::new(child_root_new_index);

        // 2. Perform merge (append all nodes from child)
        main_tree_vec.extend(child_tree_vec.into_iter());

        // 3. Fix internal pointers of the moved nodes
        // Iterate over the segment of nodes that were just added
        for i in 0..main_tree_vec.len() - offset {
            let node_index = offset + i;
            let node = main_tree_vec.get_mut(node_index).unwrap();

            // Fix Child Indices: All internal child pointers must be updated by the offset
            let mut_node = &mut node.borrow_mut();
            for child_box_option in &mut [&mut_node.child_nodes.0, &mut_node.child_nodes.1] {
                if let Some(ref child_index_box) = child_box_option {
                    let old_child_index = child_index_box.get();
                    // Mutate the usize value inside the RefCell to the new index
                    *child_index_box.value.borrow_mut() = old_child_index + offset;
                }
            }

            // Fix Parent Index
            if i > 0 {
                // For all non-root nodes of the child tree, their parent is another node within the moved segment.
                // We update this relative pointer using the offset.
                if let Some(ref parent_index_box) = mut_node.parent {
                    let old_parent_index = parent_index_box.get();
                    // Mutate the usize value inside the RefCell to the new index
                    *parent_index_box.value.borrow_mut() = old_parent_index + offset;
                }
            } else {
                // This is the root of the child tree (at new index `offset`).
                // Its new parent must be the root of the main tree (index 0).
                mut_node.parent = Some(IndexBox::new(0)); // Set parent link to main tree root
            }
        }
        
        // 4. Link the main tree's root to the new child root
        let main_root_node = main_tree_vec.get_mut(0).unwrap();
        if is_left {
            main_root_node.borrow_mut().child_nodes.0 = Some(child_root_node_ref.clone());
        } else {
            main_root_node.borrow_mut().child_nodes.1 = Some(child_root_node_ref.clone());
        }

        // Return the IndexBox for the root of the merged subtree (its index is `offset`)
        Ok(child_root_node_ref)
    }

    
    pub fn get_left_child_index(&self,ind: usize) -> Option<IndexBox> {
        let tree = self.tree_vector.borrow();
        tree.get(ind).and_then(|root| root.borrow().child_nodes.0.clone())
    }
    pub fn get_right_child_index(&self,ind: usize) -> Option<IndexBox> {
        let tree = self.tree_vector.borrow();
        tree.get(ind).and_then(|root| root.borrow().child_nodes.1.clone())
    }
}


//writen by gemeni with alot of help

// The Index and IndexBox types are already defined and used in this scope.

/// Performs a left rotation on the syntax tree stored in the vector.
///
/// P = Pivot (at pivot_index)
/// R = Right Child of P
/// RL = Left Child of R
pub fn rotate_left_helper(
    // CRITICAL CHANGE: Accept a mutable reference to the RefMut guard
    // We use &mut *tree_vec inside the function to get the &mut Vec<...>
    tree_vec_guard: &mut RefMut<'_, Vec<Rc<RefCell<SyntaxTreeNode>>>>,
    pivot_index: usize
) -> Result<(), &'static str> {
    
    // Alias the mutable vector reference for cleaner access
    let tree = &mut **tree_vec_guard; 
    
    // Variables needed outside the data gathering scope.
    let r_index: usize;
    let gp_ptr: Option<IndexBox>;
    let rl_ptr: Option<IndexBox>;
    let r_ptr: IndexBox;
    let p_index_box = IndexBox::new(pivot_index); 

    {
        // --- Data Gathering (Immutable Borrows of nodes) ---
        
        let pivot_node_ref = tree.get(pivot_index).ok_or("Pivot index is out of bounds.")?;
        
        // 1. Get R index and R pointer
        let pivot_node_borrow = pivot_node_ref.borrow();
        let r_ptr_ref = pivot_node_borrow.child_nodes.1.as_ref().ok_or("Cannot rotate left: Pivot has no right child.")?;
        r_index = r_ptr_ref.get(); 
        r_ptr = r_ptr_ref.clone();
        
        // 2. Clone Pointers for GP and RL
        let r_node_ref = tree.get(r_index).unwrap();
        
        gp_ptr = pivot_node_ref.borrow().parent.clone(); 
        rl_ptr = r_node_ref.borrow().child_nodes.0.clone();
        
    } // IMMUTABLE BORROWS of the individual nodes (Rc<RefCell<SyntaxTreeNode>>) end here.

    // --- Step 2: Update Grandparent Index Pointer (Mutating the Rc<RefCell<usize>>) ---
    if let Some(gp) = gp_ptr.as_ref() {
        let gp_index = gp.get(); 
        // Accessing the node requires an immutable borrow of the node's RefCell
        let gp_node = tree.get(gp_index).unwrap(); 

        // Find which child link of GP points to P and mutate that specific Rc<RefCell<usize>> to point to R.
        let gp_node_borrow = gp_node.borrow();
        if gp_node_borrow.child_nodes.0.as_ref().map(|idx_box| idx_box.get() == pivot_index).unwrap_or(false) {
            let gp_left_index_rc = gp_node_borrow.child_nodes.0.as_ref().unwrap().getindex();
            *gp_left_index_rc.borrow_mut() = r_index; 
        } 
        else if gp_node_borrow.child_nodes.1.as_ref().map(|idx_box| idx_box.get() == pivot_index).unwrap_or(false) {
            let gp_right_index_rc = gp_node_borrow.child_nodes.1.as_ref().unwrap().getindex();
            *gp_right_index_rc.borrow_mut() = r_index; 
        }
    }
    
    // --- Step 3: Update Right-Left Child's Parent (RL.parent = P) ---
    if let Some(rl) = rl_ptr.as_ref() {
        let rl_index = rl.get();
        // Mutate the RL node's fields.
        tree.get(rl_index).unwrap().borrow_mut().parent = Some(p_index_box.clone());
    }

    // --- Step 4: Update Pivot (P) Node's Fields (P.parent = R, P.right = RL) ---
    {
        let pivot_node_mut = tree.get(pivot_index).unwrap();

        pivot_node_mut.borrow_mut().parent = Some(r_ptr.clone()); 
        pivot_node_mut.borrow_mut().child_nodes.1 = rl_ptr.clone();
    }

    // --- Step 5: Update Right (R) Node's Fields (R.parent = GP, R.left = P) ---
    {
        let r_node = tree.get(r_index).unwrap();

        r_node.borrow_mut().parent = gp_ptr.clone(); 
        r_node.borrow_mut().child_nodes.0 = Some(p_index_box.clone());
    }
    
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub enum Opperator {
    // Structural Operators
    Terminal(tokens), // Leaf nodes: num, real, true, false, true, false
    Enclose,          // Parentheses group: ( expr )
    ArrayAccess,      // Array indexing: [ condition ]
    
    // Unary Operators
    Negate,           // Unary minus: - (equation)
    Not,              // Logical NOT: ! (condition or equation)
    
    // Assignment and Arithmetic Operators
    Assigns,          // = 
    Plus,             // +
    Minus,            // -
    Multiply,         // *
    Divide,           // /
    
    // Comparison Operators
    Equals,           // ==
    NotEquals,        // !=
    Less,             // <
    LessEquals,       // <=
    Greater,          // >
    GreaterEquals,    // >=
    
    // Logical Operators
    And,              // &&
    Or,               // ||
}

pub struct Syntaxer {
    pub controler:TreeControler,
    pub valid_trees:Vec<TreeNode>
}
impl Syntaxer {
    pub fn find_all_trees(&mut self, top_node:TreeNode) {
        // check if this tree is valid
        match top_node.rule {
            Rules::ExprToAssignEquation | Rules::ConditionToObjectHcon => {
                self.valid_trees.push(top_node);
                return ;
            }
            _ => {}
        }
        // if not continue looking
        for child in self.controler.tree_vector[top_node.this_node.unwrap()].clone().children{
            self.find_all_trees(self.controler.get_weak(child).unwrap().clone());
        }
    }

    
    pub fn make_tree(&self, top_node:TreeNode) -> Result<SyntaxTree,String>{
        // everything that can start a tree
        //println!("current rule: {}",top_node.rule.render());
        match top_node.rule {
            // every rule encountered in a condition or equation
            Rules::ArrToArrayArr => {
                // arr -> [ condition ] arr

                // check for arr -> epslon

                if self.controler.get_weak(top_node.children[3]).unwrap().rule == Rules::ArrToEpsilon {
                    // send index to the right and leave
                    let index_tree: SyntaxTree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                    let new_tree = SyntaxTree::new(Opperator::ArrayAccess);
                    new_tree.merge_right(index_tree)?;
                    return Ok(new_tree);
                }
                else {
                    let index_tree: SyntaxTree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                    // This handles the remaining or base variable (the `a` in `a[1]` or the `a[1]` in `a[1][2]`).
                    let base_tree = self.make_tree(self.controler.get_weak(top_node.children[3]).unwrap().clone())?;
                    // Create the new root node 
                    let new_tree = SyntaxTree::new(Opperator::ArrayAccess);

                    new_tree.merge_left(base_tree)?;
                    new_tree.merge_right(index_tree)?;

                    return Ok(new_tree);
                }
                
            },
            Rules::VarToIDArr => {
                // var -> id arr
                let id_node = self.controler.get_weak(top_node.children[0]).unwrap().clone();
                let mut tree = self.make_tree(id_node)?; // Base ID node

                // If arr is ArrToArrayArr, it will be merged right in ArrToArrayArr.
                // If arr is ArrToEpsilon, this is the final AST.
                // Since this rule is var, we check if the arr part is not epsilon.
                
                let arr_node = self.controler.get_weak(top_node.children[1]).unwrap().clone();
                match arr_node.rule {
                    Rules::ArrToEpsilon => return Ok(tree), // Final leaf/base case
                    _ => {
                        // The base case must be the ID, and the recursive ArrToArrayArr rule 
                        // should handle rotating the AST. If the array is not epsilon, 
                        // the entire array chain will be built recursively within the ArrToArrayArr call.
                        // Since ArrToArrayArr is the *only* rule calling Arr, we simply return the ID tree 
                        // and expect the ArrToArrayArr caller to use it.
                        // NOTE: If the grammar were `var -> id arr`, and `arr -> [c] arr | ε`, 
                        // `ArrToArrayArr` handles the structure correctly with ID being the base.
                        // We return the tree rooted at the ID/Variable, letting the parent rule handle it.
                        let arr_tree = self.make_tree(arr_node);
                         if arr_tree.is_err() {
                            return Err(arr_tree.unwrap_err().to_string());
                        }
                        let arr_unwraped = arr_tree.unwrap();
                        let _ = tree.merge_right(arr_unwraped);

                        return Ok(tree);
                    }
                }
            }
            Rules::ExprToAssignEquation => {
                // expr -> var = equation ;
                // Children: 0:var, 1:'=', 2:equation, 3:';'
                
                let var_tree = self.make_tree(self.controler.get_weak(top_node.children[0]).unwrap().clone())?;
                let eq_tree = self.make_tree(self.controler.get_weak(top_node.children[2]).unwrap().clone())?;

                let new_tree = SyntaxTree::new(Opperator::Assigns);
                new_tree.merge_left(var_tree)?;
                new_tree.merge_right(eq_tree)?;
                return Ok(new_tree);
            }
            Rules::EquationToObjectHeq => {
                // equation -> object heq
                // Children: 0:object, 1:heq
                let obj_tree = self.make_tree(self.controler.get_weak(top_node.children[0]).unwrap().clone())?;
                let heq_node = self.controler.get_weak(top_node.children[1]).unwrap().clone();
                
                match heq_node.rule {
                    Rules::HeqToEpsilon => return Ok(obj_tree),
                    _ => {
                        let heq_tree = self.make_tree(heq_node)?;
                        // Perform left-rotation logic or merge
                        heq_tree.merge_left(obj_tree)?;
                        return Ok(heq_tree); // Placeholder, requires rotation logic
                    }
                }
            }
            Rules::EquationToEncloseHeq => {
                // equation -> ( equation ) heq
                // Children: 0:'(', 1:equation, 2:')', 3:heq
                let eq_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let heq_node = self.controler.get_weak(top_node.children[3]).unwrap().clone();

                let enclosed_tree = SyntaxTree::new(Opperator::Enclose);
                enclosed_tree.merge_left(eq_tree)?;
                
                match heq_node.rule {
                    Rules::HeqToEpsilon => return Ok(enclosed_tree),
                    _ => {
                        let heq_tree = self.make_tree(heq_node)?;
                        // Placeholder: Need to correctly apply rotation/merge logic
                        enclosed_tree.merge_right(heq_tree)?;
                        return Ok(enclosed_tree); 
                    }
                }
            }
            Rules::EquationToNegate => {
                // equation -> - equation
                // Children: 0:'-', 1:equation
                let eq_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::Negate);
                new_tree.merge_left(eq_tree)?;
                return Ok(new_tree);
            }
            Rules::EquationToNegCondition => {
                // equation -> ! condition
                // Children: 0:'!', 1:condition
                let cond_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::Not);
                new_tree.merge_left(cond_tree)?;
                return Ok(new_tree);
            }
            Rules::HeqToAdd => {
                // heq -> + equation
                // Children: 0:'+', 1:equation
                let eq_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::Plus);
                new_tree.merge_right(eq_tree)?;
                
                return Ok(new_tree);
            }
            Rules::HeqToSubtract => {
                // heq -> - equation
                // Children: 0:'-', 1:equation
                let eq_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::Minus);
                new_tree.merge_right(eq_tree)?;
                return Ok(new_tree);
            }
            Rules::HeqToMultiply => {
                // heq -> * equation
                // Children: 0:'*', 1:equation
                let eq_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::Multiply);
                new_tree.merge_right(eq_tree)?;
                return Ok(new_tree);
            }
            Rules::HeqToDevide => {
                // heq -> / equation
                // Children: 0:'/', 1:equation
                let eq_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::Divide);
                new_tree.merge_right(eq_tree)?;
                return Ok(new_tree);
            }
            Rules::HeqToEpsilon => {
                // heq -> ε
                return Err("HeqToEpsilon should be handled by caller".to_string());
            }
            Rules::HeqToHcon => {
                // heq -> hcon
                return self.make_tree(self.controler.get_weak(top_node.children[0]).unwrap().clone());
            }
            Rules::ObjectToVar => {
                // object -> var
                // Children: 0:var (which resolves to id arr)
                return self.make_tree(self.controler.get_weak(top_node.children[0]).unwrap().clone());
            }
            Rules::ObjectToNum => {
                // object -> num (Terminal)
                let term_node = self.controler.get_weak(top_node.children[0]).unwrap().clone();
                match term_node.rule {
                    Rules::Terminal(t) => return Ok(SyntaxTree::new(Opperator::Terminal(t))),
                    _ => return Err("Expected Terminal for ObjectToNum".to_string()),
                };
            }
            Rules::ObjectToReal => {
                // object -> real (Terminal)
                let term_node = self.controler.get_weak(top_node.children[0]).unwrap().clone();
                match term_node.rule {
                    Rules::Terminal(t) => return Ok(SyntaxTree::new(Opperator::Terminal(t))),
                    _ => return Err("Expected Terminal for ObjectToReal".to_string()),
                };
            }
            Rules::ObjectToTrue => {
                // object -> true (Terminal)
                let term_node = self.controler.get_weak(top_node.children[0]).unwrap().clone();
                match term_node.rule {
                    Rules::Terminal(t) => return Ok(SyntaxTree::new(Opperator::Terminal(t))),
                    _ => return  Err("Expected Terminal for ObjectToTrue".to_string()),
                };
            }
            Rules::ObjectToFalse => {
                // object -> false (Terminal)
                let term_node = self.controler.get_weak(top_node.children[0]).unwrap().clone();
                match term_node.rule {
                    Rules::Terminal(t) => return Ok(SyntaxTree::new(Opperator::Terminal(t))),
                    _ => return Err("Expected Terminal for ObjectToFalse".to_string()),
                };
            }
            Rules::ConditionToObjectHcon => {
                // condition -> object hcon
                // Children: 0:object, 1:hcon
                let obj_tree = self.make_tree(self.controler.get_weak(top_node.children[0]).unwrap().clone())?;
                let hcon_node = self.controler.get_weak(top_node.children[1]).unwrap().clone();
                
                match hcon_node.rule {
                    Rules::HconToEpsilon => return Ok(obj_tree),
                    _ => {
                        let hcon_tree = self.make_tree(hcon_node)?;
                        // Placeholder, requires rotation logic
                        hcon_tree.merge_left(obj_tree)?;
                        return Ok(hcon_tree);
                    }
                }
            }
            Rules::ConditionToEnclosedHcon => {
                // condition -> ( condition ) hcon
                // Children: 0:'(', 1:condition, 2:')', 3:hcon
                let cond_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let hcon_node = self.controler.get_weak(top_node.children[3]).unwrap().clone();

                let enclosed_tree = SyntaxTree::new(Opperator::Enclose);
                enclosed_tree.merge_left(cond_tree)?;

                match hcon_node.rule {
                    Rules::HconToEpsilon => return Ok(enclosed_tree),
                    _ => {
                        let hcon_tree = self.make_tree(hcon_node)?;
                        // Placeholder: Need to correctly apply rotation/merge logic
                        enclosed_tree.merge_right(hcon_tree)?;
                        return Ok(enclosed_tree); 
                    }
                }
            }
            Rules::ConditionToNegate => {
                // condition -> ! condition
                // Children: 0:'!', 1:condition
                let cond_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::Not);
                new_tree.merge_left(cond_tree)?;
                return Ok(new_tree);
            }
            Rules::ConditionToNegEquation => {
                // condition -> - equation
                // Children: 0:'-', 1:equation
                let eq_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::Negate);
                new_tree.merge_left(eq_tree)?;
                return Ok(new_tree);
            }
            Rules::HconToEquals => {
                // hcon -> == condition
                // Children: 0:'==', 1:condition
                let cond_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::Equals);
                new_tree.merge_right(cond_tree)?;
                return Ok(new_tree);
            }
            Rules::HconToNoteq => {
                // hcon -> != condition
                // Children: 0:'!=', 1:condition
                let cond_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::NotEquals);
                new_tree.merge_right(cond_tree)?;
                return Ok(new_tree);
            }
            Rules::HconToLess => {
                // hcon -> < condition
                // Children: 0:'<', 1:condition
                let cond_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::Less);
                new_tree.merge_right(cond_tree)?;
                return Ok(new_tree);
            }
            Rules::HconToLessEq => {
                // hcon -> <= condition
                // Children: 0:'<=', 1:condition
                let cond_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::LessEquals);
                new_tree.merge_right(cond_tree)?;
                return Ok(new_tree);
            }
            Rules::HconToMore => {
                // hcon -> > condition
                // Children: 0:'>', 1:condition
                let cond_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::Greater);
                new_tree.merge_right(cond_tree)?;
                return Ok(new_tree);
            }
            Rules::HconToMoreEq => {
                // hcon -> >= condition
                // Children: 0:'>=', 1:condition
                let cond_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::GreaterEquals);
                new_tree.merge_right(cond_tree)?;
                return Ok(new_tree);
            }
            Rules::HconToAnd => {
                // hcon -> && condition
                // Children: 0:'&&', 1:condition
                let cond_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::And);
                new_tree.merge_right(cond_tree)?;
                return Ok(new_tree);
            }
            Rules::HconToOr => {
                // hcon -> || condition
                // Children: 0:'||', 1:condition
                let cond_tree = self.make_tree(self.controler.get_weak(top_node.children[1]).unwrap().clone())?;
                let new_tree = SyntaxTree::new(Opperator::Or);
                new_tree.merge_right(cond_tree)?;
                return Ok(new_tree);
            }
            Rules::HconToEpsilon => {
                // hcon -> ε
                return Err("HconToEpsilon should be handled by caller".to_string());
            }
            Rules::HconToHeq => {
                // hcon -> heq
                return self.make_tree(self.controler.get_weak(top_node.children[0]).unwrap().clone());
            }
            Rules::Terminal(t) => {
                // terminal
                return Ok(SyntaxTree::new(Opperator::Terminal(t)));
            }

            _ => {return Err(format!("invalid Rule"));}
        }
    }

    // returns the index of the new root if the root was changed
    pub fn fix_tree(&self, tree:&SyntaxTree, tree_vec: &mut RefMut<'_, Vec<Rc<RefCell<SyntaxTreeNode>>>>, index: IndexBox) -> Result<IndexBox,String> {        
        // 1. Get the current operator and rotation requirement (data gathering)
        let (current_opperator, should_rotate) = {
            
            let current_node = tree_vec.get(index.get())
                .ok_or_else(|| "Index out of bounds in fix_tree.".to_string())?;
            
            let op = current_node.borrow().opperator.clone();
            let mut rotate = false;
            
            if let Opperator::Multiply | Opperator::Divide = op {
                // Note: Calling tree.get_right_child_index here creates a redundant second immutable borrow, 
                // but is fine since it's only reading.
                let right_ind = tree_vec.get(index.get()).and_then(|root| root.borrow().child_nodes.1.clone());
                if let Some(right_idx_box) = right_ind {
                    let right_node_index = right_idx_box.get();
                    if let Some(right_node) = tree_vec.get(right_node_index) {
                        if !matches!(right_node.borrow().opperator, Opperator::Terminal(_)) {
                            rotate = true;
                        }
                    } else {
                        return Err("Right child index points to invalid node.".to_string());
                    }
                }
            }
            // if condition and there is an and /or to the right rootate
            if let Opperator::Less | Opperator::LessEquals | Opperator::Greater | Opperator::GreaterEquals | Opperator::Equals | Opperator::NotEquals | Opperator::Not = op {
                let right_ind = tree_vec.get(index.get()).and_then(|root| root.borrow().child_nodes.1.clone());
                if let Some(right_idx_box) = right_ind {
                    let right_node_index = right_idx_box.get();
                    if let Some(right_node) = tree_vec.get(right_node_index) {
                        if matches!(right_node.borrow().opperator, Opperator::And | Opperator::Or) {
                            rotate = true;
                        }
                    } else {
                        return Err("Right child index points to invalid node.".to_string());
                    }
                }
            }
            // if any math next to any condition
            if let Opperator::Plus | Opperator::Minus | Opperator::Multiply | Opperator::Divide | Opperator::Negate = op {
                let right_ind = tree_vec.get(index.get()).and_then(|root| root.borrow().child_nodes.1.clone());
                if let Some(right_idx_box) = right_ind {
                    let right_node_index = right_idx_box.get();
                    if let Some(right_node) = tree_vec.get(right_node_index) {
                        if matches!(right_node.borrow().opperator, Opperator::And | Opperator::Or | Opperator::Less | Opperator::LessEquals | Opperator::Greater | Opperator::GreaterEquals | Opperator::Equals | Opperator::NotEquals | Opperator::Not) {
                            rotate = true;
                        }
                    } else {
                        return Err("Right child index points to invalid node.".to_string());
                    }
                }
            }
            if let Opperator::Enclose  = op {
                let right_ind = tree_vec.get(index.get()).and_then(|root| root.borrow().child_nodes.1.clone());
                if let Some(right_idx_box) = right_ind {
                    let right_node_index = right_idx_box.get();
                    if let Some(right_node) = tree_vec.get(right_node_index) {
                        if matches!(right_node.borrow().opperator, Opperator::And | Opperator::Or | Opperator::Less | Opperator::LessEquals | Opperator::Greater | Opperator::GreaterEquals | Opperator::Equals | Opperator::NotEquals | Opperator::Not) {
                            rotate = true;
                        }
                    } else {
                        return Err("Right child index points to invalid node.".to_string());
                    }
                }
            }

            (op, rotate)
            
        }; // <--- vector_borrow (Immutable Borrow 1) is dropped here.
        
        //println!("Current opperator: {:?}", current_opperator);

        // 2. Perform rotation if needed
        if should_rotate {
            //println!("Left rotation happens");
            
            // Use an inner scope to ensure the mutable borrow is fully released
            // BEFORE the recursive call to fix_tree.
            let new_root_index = {
                
                // Call the new helper function
                rotate_left_helper(&mut *tree_vec, index.get())?;
                
                // Use the `tree_vec_mut` to find the parent.
                let idx = tree_vec[index.get()].borrow().parent
                    .as_ref().map(|p| p.get())
                    .ok_or_else(|| "Rotated node lost its parent pointer.".to_string())?;

                // The mutable borrow `tree_vec_mut` is dropped implicitly here 
                // as this inner scope ends. The RefCell is now unlocked.
                idx
            };
            
            // Now the recursive call can safely occur.
            return  Ok(self.fix_tree(tree,tree_vec, IndexBox::new(new_root_index))?);
        }

        // 3. Standard Recursion: Apply fix_tree to children.

        // FIX: Enforce a new scope for the left child index lookup.
        let left_ind = {
            tree_vec.get(index.get()).and_then(|root| root.borrow().child_nodes.0.clone())
        };
        if let Some(left_idx_box) = left_ind {
            self.fix_tree(tree, tree_vec,left_idx_box)?;
        }
        
        // FIX: Enforce a new scope for the right child index lookup.
        // This is the CRITICAL line that prevents the implicit borrow lifetime extension 
        // from overlapping with the nested fix_tree call's attempt to borrow_mut.
        let right_ind = {
            tree_vec.get(index.get()).and_then(|root| root.borrow().child_nodes.1.clone())
        }; // Any internal Ref is dropped here.
        
        if let Some(right_idx_box) = right_ind {
            // Line 799: No active immutable borrow from the lookup is held here.
            self.fix_tree(tree, tree_vec,right_idx_box)?;
        }

        Ok(index)
    }
}

