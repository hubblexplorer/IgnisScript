use std::process::exit;

use indexmap::IndexMap;

use crate::{
    parser::{
        NodeBinaryExpr, NodeExpr, NodeProg, NodeScope, NodeStmt,NodeTerm, Type,
    },
    tokenizer::TokenType,
};

#[derive(Clone,Debug)]
struct Var {
    stack_loc: usize,
}

#[derive(Clone,Debug)]
pub struct Generator {
    m_prog: NodeProg,
    m_output: String,
    m_stack_size: usize,
    m_vars: IndexMap<String, Var>,
    m_scopes: Vec<usize>,
    statement_count: usize,
    label_count: usize,
}

impl Generator {
    pub fn new(prog: NodeProg) -> Self {
        Generator {
            m_prog: prog,
            m_output: "global _start\n_start:\n".to_string(),
            m_stack_size: 0,
            m_vars: IndexMap::new(),
            m_scopes: Vec::new(),
            statement_count: 0,
            label_count: 0,
        }
    }

    fn get_term(&mut self, term: &NodeTerm) {
        match term {
            NodeTerm::TermIntLit(term_int_literal) => {
                let value = term_int_literal
                    .int_literal
                    .value
                    .clone()
                    .unwrap()
                    .to_string();

                self.m_output
                    .push_str(format!("\tmov rax, {}\n", value).as_str());
                self.push("rax");
            }

            NodeTerm::TermIdent(term_ident) => {
                let value = term_ident.ident.value.clone().unwrap().to_string();

                if !self.m_vars.contains_key(&value) {
                    eprintln!("Undeclared identifier: {}", value);
                    exit(-1);
                }
                let stack_loc = self.m_vars[&value].stack_loc;

                if term_ident.negated{
                    println!("Term: {:?}", term_ident);
                    if term_ident.type_ == Type::Int {
                        /*
                            mov rbx, 0
                            sub rbx, QWORD [rsp] ; Subtract the current element from the stack
                            pop rax              ; Pop the original value from the stack
                            push rbx             ; Push the negative value onto the stack */
       
                        self.m_output.push_str("\tmov rbx, 0\n");
                        self.m_output.push_str(format!("\tsub rbx, QWORD [rsp + {}]\n", (self.m_stack_size - stack_loc - 1) * 8).as_str());
                        self.pop("rax");
                        self.push("rbx");

                    } else {
                        /*
                        pop rax
                        xor rax, 1
                        push rax */
                        self.pop("rax");
                        self.m_output.push_str("\txor rax, 1\n");
                        self.push("rax");
                    }
                }

                self.push(
                    format!("QWORD [rsp + {}]", (self.m_stack_size - stack_loc - 1) * 8).as_str(),
                ); //For x86_64 if its now from the register QWORD must be used

                
            }
            NodeTerm::TermParentices(term_paren) => {
                self.get_expr(&term_paren.expr);
            }
            NodeTerm::TermBoolLit(term_bool_literal) => {
                println!("Here: {:?}", term_bool_literal.bool_literal);
                let value = term_bool_literal.bool_literal._type.clone();
                
                if value == TokenType::True {
                    self.m_output.push_str("\tmov rax, 1\n");
                } else {
                    self.m_output.push_str("\tmov rax, 0\n");
                }
                self.push("rax");
            }
            NodeTerm::TermCharLit(term_char_literal) => {
                let value = term_char_literal
                    .char_literal
                    .value
                    .clone()
                    .unwrap()
                    .to_string();
                self.m_output
                    .push_str(format!("\tmov rax, '{}'\n", value).as_str());
                self.push("rax");
            }
        }
    }

    pub fn get_expr(&mut self, expr: &NodeExpr) {
        match expr {
            NodeExpr::NodeTerm(expr_term, _) => {
                self.get_term(&expr_term);
            }
            NodeExpr::BinaryExpr(expr_binary, _) => {
                let expr_binary = expr_binary.as_ref();

                match expr_binary {
                    NodeBinaryExpr::BinaryAdd(expr_binary_add) => {
                        self.get_expr(&expr_binary_add.right);
                        self.get_expr(&expr_binary_add.left);

                        self.pop("rax");
                        self.pop("rbx");
                        self.m_output.push_str("\tadd rax, rbx\n");
                        self.push("rax");
                    }
                    NodeBinaryExpr::BinaryMulti(expr_binary_multi) => {
                        self.get_expr(&expr_binary_multi.right);
                        self.get_expr(&expr_binary_multi.left);

                        self.pop("rax");
                        self.pop("rbx");
                        self.m_output.push_str("\tmul rbx\n");
                        self.push("rax");
                    }
                    NodeBinaryExpr::BinarySub(expr_binary_sub) => {
                        self.get_expr(&expr_binary_sub.right);
                        self.get_expr(&expr_binary_sub.left);

                        self.pop("rax");
                        self.pop("rbx");
                        self.m_output.push_str("\tsub rax, rbx\n");
                        self.push("rax");
                    }
                    NodeBinaryExpr::BinaryDiv(expr_binary_div) => {
                        self.get_expr(&expr_binary_div.right);
                        self.get_expr(&expr_binary_div.left);

                        self.pop("rax");
                        self.pop("rbx");
                        self.m_output.push_str("\tdiv rbx\n");
                        self.push("rax");
                    }
                    NodeBinaryExpr::BinaryAnd(expr_binary_and) => {
                        self.get_expr(&expr_binary_and.right);
                        self.get_expr(&expr_binary_and.left);
                        self.pop("rax");
                        self.pop("rbx");
                        self.m_output.push_str("\tand rax, rbx\n");
                        self.push("rax");
                    }
                    NodeBinaryExpr::BinaryOr(expr_binary_or) => {
                        self.get_expr(&expr_binary_or.right);
                        self.get_expr(&expr_binary_or.left);
                        self.pop("rax");
                        self.pop("rbx");
                        self.m_output.push_str("\tor rax, rbx\n");
                        self.push("rax");
                    }
                    NodeBinaryExpr::BinaryLte(expr_binary_lte) => {
                        self.get_expr(&expr_binary_lte.right);
                        self.get_expr(&expr_binary_lte.left);
                        self.pop("rax");
                        self.pop("rbx");
                        self.m_output.push_str("\tcmp rax, rbx\n");
                        self.m_output.push_str("\tsetle al\n");
                        self.m_output.push_str("\tmovzx rax, al\n");
                        self.push("rax");
                    }
                    NodeBinaryExpr::BinaryLt(expr_binary_lt) => {
                        self.get_expr(&expr_binary_lt.right);
                        self.get_expr(&expr_binary_lt.left);
                        self.pop("rax");
                        self.pop("rbx");
                        self.m_output.push_str("\tcmp rax, rbx\n");
                        self.m_output.push_str("\tsetl al\n");
                        self.m_output.push_str("\tmovzx rax, al\n");
                        self.push("rax");
                    }
                    NodeBinaryExpr::BinaryGte(expr_binary_gte) => {
                        self.get_expr(&expr_binary_gte.right);
                        self.get_expr(&expr_binary_gte.left);
                        self.pop("rax");
                        self.pop("rbx");
                        self.m_output.push_str("\tcmp rax, rbx\n");
                        self.m_output.push_str("\tsetge al\n");
                        self.m_output.push_str("\tmovzx rax, al\n");
                        self.push("rax");
                    }
                    NodeBinaryExpr::BinaryGt(expr_binary_gt) => {
                        self.get_expr(&expr_binary_gt.right);
                        self.get_expr(&expr_binary_gt.left);
                        self.pop("rax");
                        self.pop("rbx");
                        self.m_output.push_str("\tcmp rax, rbx\n");
                        self.m_output.push_str("\tsetg al\n");
                        self.m_output.push_str("\tmovzx rax, al\n");
                        self.push("rax");
                    }
                    NodeBinaryExpr::BinaryEq(expr_binary_eq) =>{
                        self.get_expr(&expr_binary_eq.right);
                        self.get_expr(&expr_binary_eq.left);
                        self.pop("rax");
                        self.pop("rbx");
                        self.m_output.push_str("\tcmp rax, rbx\n");
                        self.m_output.push_str("\tsete al\n");
                        self.m_output.push_str("\tmovzx rax, al\n");
                        self.push("rax");

                    }
                    NodeBinaryExpr::BinaryNotEq(expr_binary_not_eq) =>{
                        self.get_expr(&expr_binary_not_eq.right);
                        self.get_expr(&expr_binary_not_eq.left);
                        self.pop("rax");
                        self.pop("rbx");
                        self.m_output.push_str("\tcmp rax, rbx\n");
                        self.m_output.push_str("\tsetne al\n");
                        self.m_output.push_str("\tmovzx rax, al\n");
                        self.push("rax");
                    }

                }
            }
        }
    }

    fn being_scope(&mut self) {
        self.m_scopes.push(self.m_stack_size);
       
    }

    fn end_scope(&mut self) {

        let pop_count = self.m_vars.len() - self.m_scopes.last().unwrap();
        self.m_output
            .push_str(format!("\tadd rsp, {}\n", pop_count * 8).as_str());
        self.m_stack_size -= pop_count;
        for _ in 0..pop_count {
            self.m_vars.pop();
        }
        self.m_scopes.pop();
    }

    
    fn gen_scope(&mut self, scope: &NodeScope) {
        self.being_scope();
        println!("Parsing scope: {:?}", scope);
        println!("{}", self.m_scopes.len());
        for stmt in scope.stmts.iter() {
            self.get_stmt(stmt);
            
        }
        
        self.end_scope();
    }
    fn create_label(&mut self) -> String {
        self.label_count += 1;
        let new_string = format!("label{}", self.label_count);
        return new_string;
    }
    pub fn get_stmt(&mut self, stmt: &NodeStmt) {
        match stmt {
            NodeStmt::NodeStmtExit(stmt_exit) => {
                self.get_expr(&stmt_exit.expr);
                self.m_output.push_str("\tmov rax, 60\n");
                self.pop("rdi");
                self.m_output.push_str("\tsyscall\n");
            }
            NodeStmt::NodeStmtLet(stmt_let) => {
                let value = stmt_let.ident.value.clone().unwrap();

                if self.m_vars.contains_key(&value) {
                    eprintln!("Identifier already in use: {}", value);
                    exit(-1);
                }
                self.m_vars.insert(
                    value,
                    Var {
                        stack_loc: self.m_stack_size,
                    },
                );
                self.get_expr(&stmt_let.expr);
            }
            NodeStmt::NodeStmtScope(stmt_scope) => {
                self.gen_scope(stmt_scope);
            }
            NodeStmt::NodeStmtIf(stmt_if) => {
                /*self.get_expr(&stmt_if.expr);
                self.pop("rax");
                let label = self.create_label();
                self.m_output.push_str(format!("\tcmp rax, 0\n").as_str());
                self.m_output.push_str(format!("\tjz {}\n", label).as_str());
                let scope = stmt_if.scope.clone();
                self.gen_scope(scope.as_ref());

                self.m_output.push_str(format!("{}:\n", label).as_str());*/
                //Check for else


                let end_label = self.create_label();

                self.get_expr(&stmt_if.expr);
                self.pop("rax");
  
                self.m_output.push_str(format!("\tcmp rax, 0\n").as_str());
                
                
                let stmts = self.m_prog.stmts.clone();
                let mut last_scope = stmt_if.scope.clone();



                loop {
                    self.statement_count += 1;
                    let next = &stmts[self.statement_count];
                    match next {
                    
                        NodeStmt::NodeStmtElseIf(elseif) => {
                            let new_label = self.create_label();
                            self.m_output.push_str(format!("\tjz {}\n", new_label).as_str());
                            self.gen_scope(last_scope.as_ref());
                            self.m_output.push_str(format!("\tjmp {}\n",end_label).as_str());
                            self.m_output.push_str(format!("{}:\n", new_label).as_str());
                            self.get_expr(&elseif.expr);
                            self.pop("rax");
                            self.m_output.push_str(format!("\tcmp rax, 0\n").as_str());
                            last_scope = elseif.scope.clone();
                        }
                        NodeStmt::NodeStmtElse(else_stmt) => {
                            let new_label = self.create_label();
                            self.m_output.push_str(format!("\tjz {}\n", new_label).as_str());
                            self.gen_scope(last_scope.as_ref());
                            self.m_output.push_str(format!("\tjmp {}\n",end_label).as_str());
                            self.m_output.push_str(format!("{}:\n", new_label).as_str());
                            self.gen_scope(&else_stmt.scope);
                            self.m_output.push_str(format!("{}:\n", end_label).as_str());
                            break;

                            
                        }
                        _ => {
                            self.statement_count -= 1;
                            self.m_output.push_str(format!("\tjz {}\n", end_label).as_str());
                            self.gen_scope(last_scope.as_ref());
                            self.m_output.push_str(format!("{}:\n", end_label).as_str());
                            break;
                        }
                    }
                    
                    
                    
                }
                    
                
            }
            NodeStmt::NodeStmtElseIf(_) =>{
                eprintln!("This should not happen");
                exit(-1);
            }
            NodeStmt::NodeStmtElse(_) =>{
                eprintln!("This should not happen");
                exit(-1);
            
                
            }
            NodeStmt::NodeStmtPrint(stmt_print) => {

                //The scheme for priting is always the same
                //Check if its a int, if yes print translate the value to ascii
                // If not check if its a bool, if yes print true or false
                //If not print the char

                if stmt_print.expr.get_type() == Type::Int
                    
                {
                    self.get_expr(&stmt_print.expr);
                    self.pop("rdi");
                    println!("here");

                    self.m_output.push_str("\tcall print_dec\n");
                } 
                else if stmt_print.expr.get_type() == Type::Bool
                {
                    self.get_expr(&stmt_print.expr);
                    self.pop("rdi");
                    self.m_output.push_str("\tcall print_bool\n");
                }
                
                else {
                    self.get_expr(&stmt_print.expr);
                    
                    self.m_output.push_str("\tcall print_char\n");
                    self.pop("rax");
                    
                }
             
            }
            NodeStmt::NodeStmtPrintln(stmt_println) => {
                println!(
                    "{:?}",
                    stmt_println.expr.get_type()
                );
                if stmt_println.expr.get_type() == Type::Int
                   
                {
                    self.get_expr(&stmt_println.expr);
                    self.pop("rdi");
                    self.m_output.push_str("\tcall print_dec\n");
                } 
                else if stmt_println.expr.get_type() == Type::Bool
                {
                    self.get_expr(&stmt_println.expr);
                    self.pop("rdi");
                    self.m_output.push_str("\tcall print_bool\n");
                }
                else {
                    
                    self.get_expr(&stmt_println.expr);
                    
                    self.m_output.push_str("\tcall print_char\n");
                    self.pop("rax");
                    
                }
                self.m_output.push_str("\tmov rax, 10\n"); // This is the newline character
                self.push("rax");
                self.m_output.push_str("\tcall print_char\n");
                self.pop("rax");
            

                
            }
            NodeStmt::NodeStmtAssign(stmt_assign) => {
                let value = stmt_assign.ident.value.clone().unwrap();
                if !self.m_vars.contains_key(&value) {
                    eprintln!("Identifier not in use: {}", value);
                    exit(-1);
                }
                self.get_expr(&stmt_assign.expr);
                self.pop("rax");
              
                println!("Stack loc: {}", self.m_vars[&value].stack_loc);

                self.m_output.push_str(format!("\tmov QWORD [rsp+{}], rax\n", (self.m_vars.len() - self.m_vars[&value].stack_loc -1)  * 8).as_str());
            }
            NodeStmt::NodeStmtWhile(stmt_while) => {
                let label = self.create_label(); //Create to lables, one for the start and one for the end


                let label_end = self.create_label();
                

                self.get_expr(&stmt_while.expr);
                self.pop("rax");
                self.m_output.push_str(format!("\tcmp rax, 0\n").as_str());
                self.m_output.push_str(format!("\tjz {}\n", label_end).as_str()); //If the expression is false, jump to the end


               
                self.m_output.push_str(format!("{}:\n", label).as_str()); //Start of the loop
                
                
                let scope = stmt_while.scope.clone();
                self.gen_scope(scope.as_ref());

                self.get_expr(&stmt_while.expr);
                self.pop("rax");
                self.m_output.push_str(format!("\tcmp rax, 0\n").as_str());
                self.m_output.push_str(format!("\tjne {}\n", label).as_str());//If the expression is false, jump to the start of the loop



                self.m_output.push_str(format!("{}:\n", label_end).as_str());
                
            
                
            }


            
        }
    }

    pub fn gen_prog(&mut self) -> String {
        //Create a new objet fine to nasm

        let aux = self.clone().m_prog.stmts;

        while self.statement_count < aux.len() {
            self.get_stmt(&aux[self.statement_count]);
            self.statement_count += 1;
        }


        //If no exit statement is found, add one with return 0
        self.m_output.push_str("\tmov rax, 60\n");
        self.m_output.push_str("\tmov rdi, 0\n");
        self.m_output.push_str("\tsyscall\n");

        /*
        There is a part of this code that was taken from here: https://codereview.stackexchange.com/questions/283090/print-decimal-integer 
        The conversion of negative numbers was made by the author of this code 
        */

        let print_function = "
print_dec:
    push rbp        ; callee-saved
    mov rbp, rsp    ; save sp
    
    mov rcx, 10     ; divisor base
    mov rax, rdi    ; dividend from arg0
    
    cmp rax, 0      ; check for sign
    jge L1          ; if non-negative, jump to L1
    
    ; Handle negative number:
    neg rax         ; negate the value in rax first
    push rax 
    push rcx
        
    mov rax, '-'
    push rax
    call print_char
    pop rax
        
    pop rcx
    pop rax
       
    
L1:
    xor rdx, rdx    ; zero upper dividend
    div rcx         ; unsigned divide rdx:rax by rcx
                        ; rax := quotient, rdx := remainder
    add rdx, '0'    ; convert digit to ASCII
    push rdx        ; push remainder digit
    cmp rax, 0
    jne L1          ; do while (rax != 0)
    
L2:
    call print_char
    pop rax     ; pop stack
    cmp rbp, rsp
    jne L2          ; do while (stack still has digits)
    
    pop rbp         ; restore rbp
    ret
    
print_char:
    ; Preserve the base pointer (RBP)
    

    ; Print the character using the write syscall
    mov rax, 1   ; System call for write
    mov rdi, 1   ; File descriptor for stdout
    
    mov rbx, rsp
    add rbx, 8
    
    mov rsi, rbx  
    mov rdx, 1   ; Number of bytes to write
    syscall

    ret
    
print_bool:
    ; Function to print true or false based on the value in rdi
    
    ; Compare rdi with 0
    cmp rdi, 0
    
    ; Jump to print_true label if rdi is not equal to 0
    jne print_true
    
    ; If rdi is equal to 0, print false
    mov rax, 1         ; syscall: write
    mov rdi, 1         ; file descriptor: STDOUT
    mov rsi, false_str ; pointer to the string 'false'
    mov rdx, 5         ; length of the string
    syscall

    ret

print_true:
    ; If rdi is not equal to 0, print true
    mov rax, 1         ; syscall: write
    mov rdi, 1         ; file descriptor: STDOUT
    mov rsi, true_str  ; pointer to the string 'true'
    mov rdx, 4         ; length of the string
    syscall
    ret


print_array_int:
    ; Parameters:
    ;   rax - pointer to the array
    ;   rcx - number of elements
    cmp rcx, 0  ; Check if we have reached the end of the array
    je print_array_done

    mov rdi, [rax + rcx * 8 - 8]  ; Load the current element from the array
    push rcx
    push rsi
    push rax
    
    call print_dec  ; Print the current number
    
    pop rax 
    pop rsi
    pop rcx
    dec rcx
    cmp rcx,rsi
    je print_array_done
    inc rcx
    push rcx
    push rsi
    push rax
    ; Print a space between numbers
   
    mov rax, ','  
    push rax
    call print_char
    pop rax
    pop rax
    
    pop rsi
    pop rcx
    dec rcx  ; Move to the next element in the array
    jmp print_array_int

print_array_done:
    ret
    
section .data
    true_str db 'true', 0
    false_str db 'false', 0";

        self.m_output.push_str(print_function);

        self.m_output.clone()
    }

    fn push(&mut self, reg: &str) {
        self.m_output.push_str(format!("\tpush {}\n", reg).as_str());
        self.m_stack_size += 1;
    }

    fn pop(&mut self, reg: &str) {
        self.m_output.push_str(format!("\tpop {}\n", reg).as_str());
        self.m_stack_size -= 1;
    }
}
