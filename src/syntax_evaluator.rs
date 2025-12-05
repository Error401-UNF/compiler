
use std::ops::{Deref, DerefMut};
use std::{mem, usize};
use std::rc::{Rc, Weak};
use std::cell::{RefCell, RefMut};

use super::lexical_analyzer::{tokens};
use super::symbol_table::Value;
use super::parser::{TreeNode, TreeControler, Rules,};


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
pub fn rotate_left_helper(tree_vec: &mut RefMut<'_, Vec<Rc<RefCell<SyntaxTreeNode>>>>, p_index: usize) -> Result<(), String> {
    
    let p_node = tree_vec[p_index].borrow();
    
    // R is the right child of P (the new root)
    let r_idx_box = p_node.child_nodes.1.clone().ok_or("Rotation failed: Right child (R) is missing.")?;
    let r_index = r_idx_box.get();
    let r_node = tree_vec[r_index].borrow();

    // C is the left child of R (the inner child)
    let c_idx_box = r_node.child_nodes.0.clone();
    let c_index = c_idx_box.as_ref().map(|idx| idx.get());

    // GP is the parent of P (the grandparent)
    let gp_ptr = p_node.parent.clone();

    // Drop all immutable borrows before starting mutations
    drop(p_node); 
    drop(r_node);
    
    // 2. MUTATE POINTERS

    // --- 2.1. Update GP to point to R ---
    if let Some(gp) = gp_ptr.as_ref() {
        let gp_index = gp.get();
        let mut gp_node = tree_vec[gp_index].borrow_mut(); 

        // Determine if P was the left or right child of GP
        if gp_node.child_nodes.0.as_ref().map(|idx| idx.get() == p_index).unwrap_or(false) {
            // CRITICAL: Point GP's left child to a fresh IndexBox for R
            *gp_node.child_nodes.0.as_ref().unwrap().getindex().borrow_mut() = r_index;
        } else if gp_node.child_nodes.1.as_ref().map(|idx| idx.get() == p_index).unwrap_or(false) {
            // CRITICAL: Point GP's right child to a fresh IndexBox for R
            *gp_node.child_nodes.1.as_ref().unwrap().getindex().borrow_mut() = r_index;
        }
    }

    // --- 2.2. Update C's Parent to P ---
    if let Some(c_idx) = c_index {
        tree_vec[c_idx].borrow_mut().parent = Some(IndexBox::new(p_index));
    }

    // --- 2.3. Update P (Old Root) ---
    {
        let mut p_mut = tree_vec[p_index].borrow_mut();
        // P's new parent is R
        p_mut.parent = Some(IndexBox::new(r_index)); 
        // P's new right child is C
        p_mut.child_nodes.1 = c_index.map(|idx| IndexBox::new(idx)); // CRITICAL: Fresh IndexBox
    }

    // --- 2.4. Update R (New Root) ---
    {
        let mut r_mut = tree_vec[r_index].borrow_mut();
        // R's new parent is GP
        r_mut.parent = gp_ptr; 
        // R's new left child is P
        r_mut.child_nodes.0 = Some(IndexBox::new(p_index)); // CRITICAL: Fresh IndexBox
    }

    Ok(())
}


/// Sinks a Unary Operator (U) down to the Left Child of its Binary Child (B).
/// Transformation: U( B(L, R) ) -> B( U(L), R )
pub fn sink_left_unary_helper(
    tree_vec_guard: &mut RefMut<'_, Vec<Rc<RefCell<SyntaxTreeNode>>>>,
    u_index: usize
) -> Result<(), &'static str> {
    
    let tree = &mut **tree_vec_guard;
    
    // Indices and Pointers
    let b_index: usize;
    let gp_ptr: Option<IndexBox>;
    let u_index_box = IndexBox::new(u_index);
    
    // CRITICAL: Get the raw index value of L outside any mutable block, 
    // and explicitly discard the potentially linked l_ptr IndexBox.
    let l_index_value: Option<usize>;

    // 1. Data Gathering (Immutable Borrows)
    {
        let u_node = tree.get(u_index).ok_or("Unary index out of bounds")?;
        let u_borrow = u_node.borrow();
        
        // Walk down through Enclose nodes to find the Binary Child (B) index
        let mut current_idx_box = u_borrow.child_nodes.0.clone().ok_or("Unary op has no child to sink into")?;
        loop {
            let node_index = current_idx_box.get();
            let node = tree.get(node_index).ok_or("Traversal failed in sinking")?;
            let node_op = node.borrow().opperator.clone();
            
            if matches!(node_op, Opperator::Enclose) {
                current_idx_box = node.borrow().child_nodes.0.clone().ok_or("Enclose node has no child")?;
            } else {
                b_index = node_index;
                break;
            }
        }
        
        // Get Grandparent
        gp_ptr = u_borrow.parent.clone();

        // Get Left Child of B (L) - This is where Negate will end up
        let b_node = tree.get(b_index).unwrap();
        let l_ptr_temp = b_node.borrow().child_nodes.0.clone();
        l_index_value = l_ptr_temp.as_ref().map(|l| l.get());
    } 

    // 2. Update Grandparent (GP) to point to B instead of U
    if let Some(gp) = gp_ptr.as_ref() {
        let gp_index = gp.get();
        let gp_node = tree.get(gp_index).unwrap();
        
        // This *requires* a new scope for the mutable borrow of GP
        {
            let gp_borrow = gp_node.borrow_mut();

            if gp_borrow.child_nodes.0.as_ref().map(|idx| idx.get() == u_index).unwrap_or(false) {
                // Point GP's left child to B
                *gp_borrow.child_nodes.0.as_ref().unwrap().getindex().borrow_mut() = b_index;
            } else if gp_borrow.child_nodes.1.as_ref().map(|idx| idx.get() == u_index).unwrap_or(false) {
                // Point GP's right child to B
                *gp_borrow.child_nodes.1.as_ref().unwrap().getindex().borrow_mut() = b_index;
            }
        } // Borrow of GP dropped
    }

    // 3. Update L (The node Negate is wrapping around) to point back to U
    if let Some(l_index) = l_index_value {
        tree.get(l_index).unwrap().borrow_mut().parent = Some(u_index_box.clone());
    }

    // 4. Update U (Negate)
    {
        let u_node = tree.get(u_index).unwrap();
        let mut u_mut = u_node.borrow_mut();
        u_mut.parent = Some(IndexBox::new(b_index)); // Parent is now B (Fresh IndexBox)
        
        // CRITICAL FIX: Create a new IndexBox for L's index value. 
        // DO NOT reuse the IndexBox cloned from B's original child pointer.
        u_mut.child_nodes.0 = l_index_value.map(IndexBox::new); // Child is now L (Fresh IndexBox)
        u_mut.child_nodes.1 = None;
    }

    // 5. Update B (The Binary Op)
    {
        let b_node = tree.get(b_index).unwrap();
        let mut b_mut = b_node.borrow_mut();
        b_mut.parent = gp_ptr; // Parent is now GP (Reused original GP Rc)
        b_mut.child_nodes.0 = Some(u_index_box); // Left child is now U (Original IndexBox)
    }

    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub enum Opperator {
    // Structural Operators
    Terminal(tokens), // Leaf nodes: num, real, bool, true, false,
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

    // bit shift opperators
    ShiftLeft,         // <<
    ShiftRight,        // >>
}
impl Opperator {
    pub fn unwrap_terminal(&self) -> Option<tokens>{
        match self {
            Opperator::Terminal(t) => {return Some(t.clone());}
            _ => {return None}
        }
    }
}

pub struct Syntaxer {
    pub controler:TreeControler,
    pub valid_trees:Vec<TreeNode>
}
impl Syntaxer {
    pub fn find_all_trees(&mut self, top_node:TreeNode) {
        // check if this tree is valid
        match top_node.rule.clone() {
            x if Self::detect_ast_start_node(&x) => {
                self.valid_trees.push(top_node);
                return ;
            }
            Rules::DeclToVar => {
                return; // skip these
            }
            _ => {}
        }
        // if not continue looking
        for child in self.controler.tree_vector[top_node.this_node.unwrap()].clone().children{
            self.find_all_trees(self.controler.clone().get_weak(child).unwrap().clone());
        }
    }

    pub fn detect_ast_start_node(rule:&Rules)-> bool {
        return matches!(rule, 
            // expressions
            Rules::ExprToAssignEquation |
            // eq
            Rules::EquationToObjectHeq |
            Rules::EquationToEncloseHeq |
            Rules::EquationToNegate |
            Rules::EquationToNegCondition |

            // cond
            Rules::ConditionToObjectHcon |
            Rules::ConditionToEnclosedHcon |
            Rules::ConditionToNegate |
            Rules::ConditionToNegEquation
        );
    }
    pub fn make_tree(&mut self, top_node:TreeNode) -> Result<SyntaxTree,String>{
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
            }
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

    
    pub fn fix_tree(&self, tree:&SyntaxTree, tree_vec: &mut RefMut<'_, Vec<Rc<RefCell<SyntaxTreeNode>>>>, index: IndexBox) -> Result<IndexBox,String> {        
        
        // 1. Data Gathering for Operations (Read-only Phase)
        let (op, should_rotate, _should_sink_unary) = { // removed check for should_sink_unary
            let imutable_tree = tree_vec.clone();
            let current_node = imutable_tree.get(index.get())
                .ok_or_else(|| "Index out of bounds in fix_tree.".to_string())?;
            
            let op = current_node.borrow().opperator.clone();
            let mut rotate = false;

            // --- CHECK FOR ROTATION (Structural and Precedence Fixes - Highest Priority) ---
            
            // 0. CRITICAL STRUCTURAL FIX: Enclose over any other non-Terminal operator
            if let Opperator::Enclose  = op {
                let right_ind = current_node.borrow().child_nodes.1.clone();
                if let Some(right_idx_box) = right_ind {
                    if let Some(right_node) = imutable_tree.get(right_idx_box.get()) {
                        if !matches!(right_node.borrow().opperator, Opperator::Terminal(_)) {
                            rotate = true;
                        } 
                    }
                }
            }
            
            // 0b. CRITICAL FIX: Unary Over Binary (Sinking Replacement)
            // If a unary op is over a binary op, we force a left rotation to push the unary op down.
            if !rotate {
                if matches!(op, Opperator::Negate | Opperator::Not) {
                    let left_ind = current_node.borrow().child_nodes.0.clone();
                    if let Some(l_idx_box) = left_ind {
                        if let Some(left_node) = imutable_tree.get(l_idx_box.get()) {
                            let left_op = left_node.borrow().opperator.clone();
                            // Check if the left child is a binary operator that the unary op should sink over
                            if matches!(left_op, Opperator::Plus | Opperator::Minus | Opperator::Multiply | Opperator::Divide | Opperator::And | Opperator::Or | Opperator::Equals | Opperator::NotEquals | Opperator::Less | Opperator::LessEquals | Opperator::Greater | Opperator::GreaterEquals) {
                                rotate = true; // Force rotation to achieve sinking
                            }
                        }
                    }
                }
            }


            // 1. Left-Associativity (e.g., Multiply/Divide over Multiply/Divide)
            if !rotate {
                if let Opperator::Multiply | Opperator::Divide = op { 
                    let right_ind = current_node.borrow().child_nodes.1.clone();
                    if let Some(right_idx_box) = right_ind {
                        if let Some(right_node) = imutable_tree.get(right_idx_box.get()) {
                            let right_op = right_node.borrow().opperator.clone();
                            if matches!(right_op, Opperator::Multiply | Opperator::Divide) { 
                                rotate = true;
                            }
                        }
                    }
                }
            }

            // 1a. Standard Arithmetic Precedence Rotation (Divide/Multiply over Plus/Minus)
            if !rotate {
                if let Opperator::Multiply | Opperator::Divide = op { 
                    let right_ind = current_node.borrow().child_nodes.1.clone();
                    if let Some(right_idx_box) = right_ind {
                        if let Some(right_node) = imutable_tree.get(right_idx_box.get()) {
                            let right_op = right_node.borrow().opperator.clone();
                            if matches!(right_op, Opperator::Plus | Opperator::Minus) { 
                                rotate = true;
                            }
                        }
                    }
                }
            }
            
            // 2. Conditions over Logical (Comparison over AND/OR)
            if !rotate {
                if let Opperator::Less | Opperator::LessEquals | Opperator::Greater | Opperator::GreaterEquals | Opperator::Equals | Opperator::NotEquals | Opperator::Not = op {
                    let right_ind = imutable_tree.get(index.get()).and_then(|root| root.borrow().child_nodes.1.clone());
                    if let Some(right_idx_box) = right_ind {
                        let right_node_index = right_idx_box.get();
                        if let Some(right_node) = imutable_tree.get(right_node_index) {
                            if matches!(right_node.borrow().opperator, Opperator::And | Opperator::Or) {
                                rotate = true;
                            }
                        } 
                    }
                }
            }
            
            // 3. Math over Conditions/Logical (Math over Comparison/Logical)
            if !rotate {
                if let Opperator::Plus | Opperator::Minus | Opperator::Multiply | Opperator::Divide | Opperator::Negate = op { 
                    let right_ind = imutable_tree.get(index.get()).and_then(|root| root.borrow().child_nodes.1.clone());
                    if let Some(right_idx_box) = right_ind {
                        let right_node_index = right_idx_box.get();
                        if let Some(right_node) = imutable_tree.get(right_node_index) {
                            if matches!(right_node.borrow().opperator, Opperator::And | Opperator::Or | Opperator::Less | Opperator::LessEquals | Opperator::Greater | Opperator::GreaterEquals | Opperator::Equals | Opperator::NotEquals | Opperator::Not) {
                                rotate = true;
                            }
                        } 
                    }
                }
            }

            (op, rotate, false) // sink is now always false
        }; 

        // 2. Perform Rotation (MUST BE FIRST)
        if should_rotate {
            let old_root_index = index.get();
            let new_root_index = {
                rotate_left_helper(&mut *tree_vec, old_root_index)?;
                
                let idx = tree_vec[old_root_index].borrow().parent
                    .as_ref().map(|p| p.get())
                    .ok_or_else(|| "Rotated node lost its parent pointer.".to_string())?;
                idx
            };
            // Recursively fix the new root immediately
            //println!("recurse path 2 (Rotation complete from index {})", old_root_index);
            return Ok(self.fix_tree(tree, tree_vec, IndexBox::new(new_root_index))?);
        }
        
        // Removed Sinking Check (3. Perform Sinking)

        // 4. Standard Recursion on Children
        
        let left_ind = {
            tree_vec.get(index.get()).and_then(|root| root.borrow().child_nodes.0.clone())
        };
        if let Some(left_idx_box) = left_ind {
            //println!("recurse path 3: {} and {}",index.get(),left_idx_box.get());
            self.fix_tree(tree, tree_vec, left_idx_box)?;
        }

        let right_ind = {
            tree_vec.get(index.get()).and_then(|root| root.borrow().child_nodes.1.clone())
        };
        
        if let Some(right_idx_box) = right_ind {
            //println!("recurse path 4: {} and {}",index.get(),right_idx_box.get());
            self.fix_tree(tree, tree_vec, right_idx_box)?;
        }

        Ok(index)
    }

    pub fn optimizer(&self, tree_vec: &mut RefMut<'_, Vec<Rc<RefCell<SyntaxTreeNode>>>>, index: IndexBox) -> Result<IndexBox,String> {
        
        // 1. Data Gathering and Analysis
        let (op_clone, right_ind_option, right_node_index, ispower, inverted, number_to_shift, offset) = {
            
            let imutable_tree = tree_vec.clone();
            let current_node = imutable_tree.get(index.get())
                .ok_or_else(|| "Index out of bounds in optimizer.".to_string())?;

            let op_clone = current_node.borrow().opperator.clone();
            let right_ind_option = current_node.borrow().child_nodes.1.clone();
            
            let mut ispower = false;
            let mut inverted = false;
            let mut number_to_shift = 0;
            let mut offset = 0;
            let mut right_node_index = 0;
            let mut right_node_op = None;

            if let Some(right_idx_box) = right_ind_option.clone() {
                right_node_index = right_idx_box.get();
                if let Some(right_node) = imutable_tree.get(right_node_index) {
                    right_node_op = Some(right_node.borrow().opperator.clone());
                }
            }

            // --- Core Analysis Logic (Same as before) ---
            if let Some(right_op) = right_node_op {
                if matches!(op_clone, Opperator::Multiply | Opperator::Divide) {
                    if matches!(right_op, Opperator::Terminal(_)){
                        match right_op.clone() {
                            Opperator::Terminal(tok) => {
                                if matches!(tok, tokens::Number(_)){
                                    let n = tok.extract_int_value().unwrap();
                                    if n != 0 {
                                        let log2 = f64::log2(n as f64);
                                        if log2.floor() == log2{
                                            ispower = true;
                                            inverted = false;
                                            number_to_shift = log2 as i32;
                                            offset = 0;
                                            println!("1number in question is: {}",n);
                                        } else {
                                            let minus1 = f64::log2((n-1) as f64);
                                            let plus1 = f64::log2((n+1) as f64);
                                            if n > 1 && minus1.floor() == minus1{
                                                ispower = true;
                                                inverted = false;
                                                number_to_shift = minus1 as i32;
                                                offset = 1;
                                            } else if plus1.floor() == plus1 {
                                                ispower = true;
                                                inverted = false;
                                                number_to_shift = plus1 as i32;
                                                offset = -1;
                                            }
                                        }
                                    }
                                } else if matches!(tok, tokens::Real(r)) {
                                    let r = tok.extract_float_value().unwrap();
                                    if r > 0.0 {
                                        let log2 = f64::log2(r);
                                        if (log2.fract()).abs() < 1e-9 { 
                                            ispower = true;
                                            inverted = r < 1.0;
                                            number_to_shift = log2.round() as i32;
                                            offset = 0;
                                            println!("2number in question is: {}",r);
                                        } else {
                                            let minus1 = f64::log2(r-1.0);
                                            let plus1 = f64::log2(r+1.0);
                                            if r > 1.0 && (minus1.fract()).abs() < 1e-9 {
                                                ispower = true;
                                                inverted = r < 1.0;
                                                number_to_shift = minus1.round() as i32;
                                                offset = 1;
                                            } else if (plus1.fract()).abs() < 1e-9 {
                                                ispower = true;
                                                inverted = r < 1.0;
                                                number_to_shift = plus1.round() as i32;
                                                offset = -1;
                                            }
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            
            // Use right_ind_option for return
            (op_clone, right_ind_option, right_node_index, ispower, inverted, number_to_shift, offset)
        };

        // 2. Perform Strength Reduction Transformation (Mutation Phase)
        if ispower {
            // Handle Left Shift cases
            if (matches!(op_clone, Opperator::Multiply) && !inverted) || (matches!(op_clone, Opperator::Divide) && inverted){
                if offset == 0 {
                    // X * 2^n -> X << n (No offset)
                    
                    // Change top node op to ShiftLeft
                    tree_vec[index.get()].borrow_mut().opperator = Opperator::ShiftLeft;

                    // Change right node op to Terminal(Number(n))
                    let mut mut_right = tree_vec[right_node_index].borrow_mut();
                    mut_right.opperator = Opperator::Terminal(tokens::Number(number_to_shift.abs()));
                } else {
                    // X * (2^n +/- 1) -> (X << n) +/- X
                    
                    let original_left_ind = tree_vec[index.get()].borrow().child_nodes.0.clone().unwrap();
                    let shift_op = number_to_shift.abs();
                    
                    // Get the left operand's root operator *before* any mutation
                    let x_opperator = tree_vec[original_left_ind.get()].borrow().opperator.clone();
                    let right_ind_shift_left = right_ind_option.clone().unwrap();

                    // 1. Create the new nodes needed for the restructuring (Shift amount and X's new representation)
                    
                    // Left child of ShiftLeft (X, using only the copied operator, WARNING: FLAGGED UNSAFE/FLAWED)
                    let left_node_for_shift = SyntaxTreeNode::child(right_ind_shift_left.clone(), x_opperator, Value::Null);
                    let left_box = Rc::new(RefCell::new(left_node_for_shift));

                    // Right child of ShiftLeft (shift amount 'n')
                    let right_node_shift_val = SyntaxTreeNode::child(right_ind_shift_left.clone(), Opperator::Terminal(tokens::Number(shift_op)), Value::Null);
                    let right_box = Rc::new(RefCell::new(right_node_shift_val));
                    
                    // Store new nodes temporarily with their new indices
                    let mut new_nodes = Vec::new();
                    let left_ind = IndexBox::new(tree_vec.len());
                    new_nodes.push((Rc::clone(&left_box), left_ind.clone()));

                    let right_ind_shift_val = IndexBox::new(tree_vec.len() + 1);
                    new_nodes.push((Rc::clone(&right_box), right_ind_shift_val.clone()));

                    
                    {
                        let mut mut_top = tree_vec[index.get()].borrow_mut();
                        if offset == 1{
                            mut_top.opperator = Opperator::Plus;
                        } else {
                            mut_top.opperator = Opperator::Minus;
                        }
                        let mut mut_right = tree_vec[right_node_index].borrow_mut();
                        mut_right.opperator = Opperator::ShiftLeft;

                        mut_right.child_nodes = (
                            Some(left_ind),
                            Some(right_ind_shift_val)
                        );
                    }

                    // 3. Insert new nodes (Now safe to push to tree_vec)
                    for (node_rc, _) in new_nodes {
                        tree_vec.push(node_rc);
                    }
                }
            }
            else if (matches!(op_clone, Opperator::Divide) && !inverted) || (matches!(op_clone, Opperator::Multiply) && inverted){
                // Right Shift (X / 2^n) or (X * 2^-n)
                if offset == 0 {
                    // Change top node op to ShiftRight
                    tree_vec[index.get()].borrow_mut().opperator = Opperator::ShiftRight;

                    // Change right node op to Terminal(Number(n))
                    let mut mut_right = tree_vec[right_node_index].borrow_mut();
                    mut_right.opperator = Opperator::Terminal(tokens::Number(number_to_shift.abs()));
                } else {
                    // Division optimization for 2^n +/- 1 (Still unimplemented as requested)
                    return Err(format!("Division optimization for 2^n +/- 1 is unimplemented. Stats: offset {}. number to shift {}", offset, number_to_shift));
                }
            }
            println!("optimization Done!!");
            tree_vec[index.get()].borrow().render_node(tree_vec, 0, false);
            println!("stats: offset {}. number to shift {}\n", offset, number_to_shift);
        }

        // 3. Standard Recursion on Children
        
        let left_ind = {
            tree_vec.get(index.get()).and_then(|root| root.borrow().child_nodes.0.clone())
        };
        if let Some(left_idx_box) = left_ind {
            self.optimizer(tree_vec, left_idx_box)?;
        }

        let right_ind = {
            tree_vec.get(index.get()).and_then(|root| root.borrow().child_nodes.1.clone())
        };
        if let Some(right_idx_box) = right_ind {
            self.optimizer(tree_vec, right_idx_box)?;
        }

        Ok(index)
    }

}

