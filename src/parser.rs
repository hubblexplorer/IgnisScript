use std::process::exit;

use crate::{
    tokenizer::Token,
    tokenizer::{bin_precedence, TokenType},
};

#[derive(Debug, Clone, PartialEq)]
pub struct NodeExprIntLit {
    pub int_literal: Token,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeExprBoolLit {
    pub bool_literal: Token,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeExprCharLit {
    pub char_literal: Token,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeExprIdent {
    pub ident: Token,
    pub type_: Type,
    pub negated: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeBinaryAdd {
    pub left: NodeExpr,
    pub right: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeBinaryMulti {
    pub left: NodeExpr,
    pub right: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeBinaryAnd {
    pub left: NodeExpr,
    pub right: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeBinaryOr {
    pub left: NodeExpr,
    pub right: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeBinaryEq {
    pub left: NodeExpr,
    pub right: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeBinaryLt {
    pub left: NodeExpr,
    pub right: NodeExpr,
}
#[derive(Debug, Clone, PartialEq)]
pub struct NodeBinaryLte {
    pub left: NodeExpr,
    pub right: NodeExpr,
}
#[derive(Debug, Clone, PartialEq)]
pub struct NodeBinaryGt {
    pub left: NodeExpr,
    pub right: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeBinaryGte {
    pub left: NodeExpr,
    pub right: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeBinaryNotEq {
    pub left: NodeExpr,
    pub right: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeBinaryExpr {
    BinaryAdd(NodeBinaryAdd),
    BinaryMulti(NodeBinaryMulti),
    BinarySub(NodeBinarySub),
    BinaryDiv(NodeBinaryDiv),
    BinaryAnd(NodeBinaryAnd),
    BinaryOr(NodeBinaryOr),
    BinaryEq(NodeBinaryEq),
    BinaryLt(NodeBinaryLt),
    BinaryGt(NodeBinaryGt),
    BinaryLte(NodeBinaryLte),
    BinaryGte(NodeBinaryGte),
    BinaryNotEq(NodeBinaryNotEq),
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeUniNot {
    pub expr: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeUniNeg {
    pub expr: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeBinaryDiv {
    pub left: NodeExpr,
    pub right: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeBinarySub {
    pub left: NodeExpr,
    pub right: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeTerm {
    TermIntLit(NodeExprIntLit),
    TermBoolLit(NodeExprBoolLit),
    TermCharLit(NodeExprCharLit),
    TermIdent(NodeExprIdent),
    TermParentices(Box<NodeTermParentices>),
}

impl NodeTerm {
    pub fn get_type(&self) -> Type {
        match self {
            NodeTerm::TermIntLit(t) => t.type_.clone(),
            NodeTerm::TermBoolLit(t) => t.type_.clone(),
            NodeTerm::TermIdent(t) => t.type_.clone(),
            NodeTerm::TermParentices(t) => t.expr.get_type(),
            NodeTerm::TermCharLit(t) => t.type_.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeTermParentices {
    pub expr: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeExpr {
    NodeTerm(NodeTerm, Type),
    BinaryExpr(Box<NodeBinaryExpr>, Type),
}

impl NodeExpr {
    pub fn get_type(&self) -> Type {
        match self {
            NodeExpr::NodeTerm(_, t) => t.clone(),
            NodeExpr::BinaryExpr(_, t) => t.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeProg {
    pub stmts: Vec<NodeStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeStmtExit {
    pub expr: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeStmtLet {
    pub ident: Token,
    pub expr: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeStmtIf {
    pub expr: NodeExpr,
    pub scope: Box<NodeScope>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeStmtElseIf {
    pub expr: NodeExpr,
    pub scope: Box<NodeScope>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeStmtElse {
    pub scope: Box<NodeScope>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeStmtAssign {
    pub ident: Token,
    pub expr: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeStmtWhile {
    pub expr: NodeExpr,
    pub scope: Box<NodeScope>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeStmt {
    NodeStmtExit(NodeStmtExit),
    NodeStmtLet(NodeStmtLet),
    NodeStmtScope(Box<NodeScope>),
    NodeStmtIf(NodeStmtIf),
    NodeStmtElseIf(NodeStmtElseIf),
    NodeStmtElse(NodeStmtElse),
    NodeStmtPrint(NodeStmtPrint),
    NodeStmtPrintln(NodeStmtPrintln),
    NodeStmtAssign(NodeStmtAssign),
    NodeStmtWhile(NodeStmtWhile),
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeStmtPrint {
    pub expr: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeStmtPrintln {
    pub expr: NodeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeScope {
    pub stmts: Vec<NodeStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    pub tokens: Vec<Token>,
    pub index: usize,
    pub variables_types: Vec<(Token, Type)>,
    pub prog: NodeProg,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Char,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            index: 0,
            variables_types: vec![],
            prog: NodeProg { stmts: vec![] },
        }
    }

    fn parse_term(&mut self, type_: Type) -> Option<NodeTerm> {
        let mut negate = self.peek(0);

        if negate.is_some()
            && (negate.clone().unwrap()._type == TokenType::Minus
                || negate.clone().unwrap()._type == TokenType::Exclamation)
        {
            self.consume(1);
        } else {
            negate = None;
        }

        if let Some(t) = self.peek(0) {
            if t._type == TokenType::IntLiteral {
                let mut int_literal = self.consume(1);
                if negate.is_some() && negate.unwrap()._type == TokenType::Minus {
                    int_literal.value = Some(format!("-{}", int_literal.value.unwrap()));
                }
                Some(NodeTerm::TermIntLit(NodeExprIntLit {
                    int_literal,
                    type_: Type::Int,
                }))
            } else if t._type == TokenType::True {
                let mut bool_literal = self.consume(1);
                if negate.is_some() && negate.unwrap()._type == TokenType::Exclamation {
                    bool_literal.value = Some(format!("false"));
                    bool_literal._type = TokenType::False;
                }
                Some(NodeTerm::TermBoolLit(NodeExprBoolLit {
                    bool_literal,
                    type_: Type::Bool,
                }))
            } else if t._type == TokenType::False {
                let mut bool_literal = self.consume(1);
                if negate.is_some() && negate.unwrap()._type == TokenType::Exclamation {
                    bool_literal.value = Some(format!("true"));
                    bool_literal._type = TokenType::True;
                }
                Some(NodeTerm::TermBoolLit(NodeExprBoolLit {
                    bool_literal,
                    type_: Type::Bool,
                }))
            } else if t._type == TokenType::CharLiteral {
                let char_literal = self.consume(1);
                Some(NodeTerm::TermCharLit(NodeExprCharLit {
                    char_literal,
                    type_: Type::Char,
                }))
            } else if t._type == TokenType::Ident {
                let ident = self.consume(1);

                for i in 0..self.variables_types.len() {
                    if self.variables_types[i].0.value == ident.value {
                        if negate.is_none() {
                            return Some(NodeTerm::TermIdent(NodeExprIdent {
                                ident,
                                type_: self.variables_types[i].1.clone(),
                                negated: false,
                            }));
                        } else {
                            if self.variables_types[i].1 == Type::Int {
                                if negate.unwrap()._type == TokenType::Minus {
                                    return Some(NodeTerm::TermIdent(NodeExprIdent {
                                        ident,
                                        type_: Type::Int,
                                        negated: true,
                                    }));
                                } else {
                                    eprintln!("Expected -");
                                    exit(-1);
                                }
                            } else if self.variables_types[i].1 == Type::Bool {
                                if negate.unwrap()._type == TokenType::Exclamation {
                                    return Some(NodeTerm::TermIdent(NodeExprIdent {
                                        ident,
                                        type_: Type::Bool,
                                        negated: true,
                                    }));
                                } else {
                                    eprintln!("Expected !");
                                    exit(-1);
                                }
                            } else {
                                eprintln!("Expected - or !");
                                exit(-1);
                            }
                        }
                    }
                }

                Some(NodeTerm::TermIdent(NodeExprIdent {
                    ident,
                    type_,
                    negated: false,
                }))
            } else if t._type == TokenType::OpenParent {
                self.consume(1);

                if let Some(expr) = self.parse_expr(0, type_) {
                    if let Some(t) = self.peek(0) {
                        if t._type == TokenType::CloseParent {
                            self.consume(1);
                            return Some(NodeTerm::TermParentices(Box::new(NodeTermParentices {
                                expr,
                            })));
                        } else {
                            println!("Expected )");
                            exit(-1);
                        }
                    } else {
                        println!("Expected expression");
                        exit(-1);
                    }
                } else {
                    return None;
                }
            } else {
                return None;
            }
        } else {
            return None;
        }
    }

    pub fn parse_expr(&mut self, precedence: i32, type_: Type) -> Option<NodeExpr> {
        if let Some(term_left) = self.parse_term(type_.clone()) {
            let mut expr_left = NodeExpr::NodeTerm(term_left.clone(), term_left.get_type());

            loop {
                if let Some(current_token) = self.peek(0) {
                    if let Some(bin_prec) = bin_precedence(&current_token._type) {
                        if bin_prec < precedence {
                            break;
                        }
                        let op = self.consume(1);

                        let mut op_2 = None;
                        if let Some(aux) = self.peek(0) {
                            if aux._type == TokenType::Eq {
                                op_2 = Some(self.consume(1));
                            }
                        }

                        let next_min_prec = bin_prec + 1;

                        if let Some(expr_right) = self.parse_expr(next_min_prec, type_.clone()) {
                            if expr_left.get_type() == expr_right.get_type() {
                                if expr_left.get_type() == Type::Int {
                                    match op._type {
                                        TokenType::Plus => {
                                            let expr_left2 = expr_left;
                                            let expr = NodeExpr::BinaryExpr(
                                                Box::new(NodeBinaryExpr::BinaryAdd(
                                                    NodeBinaryAdd {
                                                        left: expr_left2,
                                                        right: expr_right,
                                                    },
                                                )),
                                                Type::Int,
                                            );
                                            expr_left = expr;
                                        }
                                        TokenType::Star => {
                                            let expr_left2 = expr_left;
                                            let expr = NodeExpr::BinaryExpr(
                                                Box::new(NodeBinaryExpr::BinaryMulti(
                                                    NodeBinaryMulti {
                                                        left: expr_left2,
                                                        right: expr_right,
                                                    },
                                                )),
                                                Type::Int,
                                            );
                                            expr_left = expr;
                                        }
                                        TokenType::Minus => {
                                            let expr_left2 = expr_left;
                                            let expr = NodeExpr::BinaryExpr(
                                                Box::new(NodeBinaryExpr::BinarySub(
                                                    NodeBinarySub {
                                                        left: expr_left2,
                                                        right: expr_right,
                                                    },
                                                )),
                                                Type::Int,
                                            );
                                            expr_left = expr;
                                        }
                                        TokenType::Slash => {
                                            let expr_left2 = expr_left;
                                            let expr = NodeExpr::BinaryExpr(
                                                Box::new(NodeBinaryExpr::BinaryDiv(
                                                    NodeBinaryDiv {
                                                        left: expr_left2,
                                                        right: expr_right,
                                                    },
                                                )),
                                                Type::Int,
                                            );
                                            expr_left = expr;
                                        }
                                        TokenType::LessThan => {
                                            if op_2.is_some()
                                                && op_2.unwrap()._type == TokenType::Eq
                                            {
                                                let expr_left2 = expr_left;
                                                let expr = NodeExpr::BinaryExpr(
                                                    Box::new(NodeBinaryExpr::BinaryLte(
                                                        NodeBinaryLte {
                                                            left: expr_left2,
                                                            right: expr_right,
                                                        },
                                                    )),
                                                    Type::Bool,
                                                );
                                                expr_left = expr;
                                            } else {
                                                let expr_left2 = expr_left;
                                                let expr = NodeExpr::BinaryExpr(
                                                    Box::new(NodeBinaryExpr::BinaryLt(
                                                        NodeBinaryLt {
                                                            left: expr_left2,
                                                            right: expr_right,
                                                        },
                                                    )),
                                                    Type::Bool,
                                                );
                                                expr_left = expr;
                                            }
                                        }
                                        TokenType::GreaterThan => {
                                            if op_2.is_some()
                                                && op_2.unwrap()._type == TokenType::Eq
                                            {
                                                let expr_left2 = expr_left;
                                                let expr = NodeExpr::BinaryExpr(
                                                    Box::new(NodeBinaryExpr::BinaryGte(
                                                        NodeBinaryGte {
                                                            left: expr_left2,
                                                            right: expr_right,
                                                        },
                                                    )),
                                                    Type::Bool,
                                                );
                                                expr_left = expr;
                                            } else {
                                                let expr_left2 = expr_left;
                                                let expr = NodeExpr::BinaryExpr(
                                                    Box::new(NodeBinaryExpr::BinaryGt(
                                                        NodeBinaryGt {
                                                            left: expr_left2,
                                                            right: expr_right,
                                                        },
                                                    )),
                                                    Type::Bool,
                                                );
                                                expr_left = expr;
                                            }
                                        }
                                        TokenType::Eq => {
                                            if op_2.is_some()
                                                && op_2.unwrap()._type == TokenType::Eq
                                            {
                                                let expr_left2 = expr_left;
                                                let expr = NodeExpr::BinaryExpr(
                                                    Box::new(NodeBinaryExpr::BinaryEq(
                                                        NodeBinaryEq {
                                                            left: expr_left2,
                                                            right: expr_right,
                                                        },
                                                    )),
                                                    Type::Bool,
                                                );
                                                expr_left = expr;
                                            } else {
                                                eprintln!("Error: Unbounded '='");
                                                exit(-1);
                                            }
                                        }
                                        TokenType::Exclamation => {
                                            if op_2.is_some()
                                                && op_2.unwrap()._type == TokenType::Eq
                                            {
                                                let expr_left2 = expr_left;
                                                let expr = NodeExpr::BinaryExpr(
                                                    Box::new(NodeBinaryExpr::BinaryNotEq(
                                                        NodeBinaryNotEq {
                                                            left: expr_left2,
                                                            right: expr_right,
                                                        },
                                                    )),
                                                    Type::Bool,
                                                );
                                                expr_left = expr;
                                            } else {
                                                eprintln!("Error: Unbounded '!'");
                                                exit(-1);
                                            }
                                        }
                                        _ => {
                                            eprintln!("Error: Expected expression");
                                            exit(-1);
                                        }
                                    }
                                } else if expr_left.get_type() == Type::Bool {
                                    match op._type {
                                        TokenType::And => {
                                            let expr_left2 = expr_left;
                                            let expr = NodeExpr::BinaryExpr(
                                                Box::new(NodeBinaryExpr::BinaryAnd(
                                                    NodeBinaryAnd {
                                                        left: expr_left2,
                                                        right: expr_right,
                                                    },
                                                )),
                                                Type::Bool,
                                            );
                                            expr_left = expr;
                                        }
                                        TokenType::Or => {
                                            let expr_left2 = expr_left;
                                            let expr = NodeExpr::BinaryExpr(
                                                Box::new(NodeBinaryExpr::BinaryOr(NodeBinaryOr {
                                                    left: expr_left2,
                                                    right: expr_right,
                                                })),
                                                Type::Bool,
                                            );
                                            expr_left = expr;
                                        }
                                        _ => {
                                            eprintln!("Error: Expected expression");
                                            exit(-1);
                                        }
                                    }
                                }
                            } else {
                                eprintln!(
                                    "Error: Type mismatch {:?} and {:?}",
                                    expr_left.get_type(),
                                    expr_right.get_type()
                                );
                                exit(-1);
                            }
                        } else {
                            eprintln!("Error: Unable to parse expression");
                            exit(-1);
                        }
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            return Some(expr_left);
        } else {
            return None;
        }
    }

    fn parse_scope(&mut self, type_: Type) -> Option<NodeScope> {
        if let Some(t) = self.peek(0) {
            if t._type == TokenType::OpenCurly {
                self.consume(1);
                let mut stmts = Vec::new();

                while let Some(stmt) = self.parse_stmt(type_.clone()) {
                    stmts.push(stmt);
                }

                if let Some(t) = self.peek(0) {
                    if t._type == TokenType::CloseCurly {
                        self.consume(1);
                        return Some(NodeScope { stmts: stmts });
                    } else {
                        eprintln!("Error: Expected '}}'");
                        exit(-1);
                    }
                } else {
                    eprintln!("Error: Expected '}}'");
                    exit(-1);
                }
            }
        }
        return None;
    }

    pub fn parse_stmt(&mut self, type_: Type) -> Option<NodeStmt> {
        let mut node = None;

        if let Some(t) = self.peek(0) {
            if t._type == TokenType::OpenCurly {
                if let Some(scope) = self.parse_scope(type_.clone()) {
                    return Some(NodeStmt::NodeStmtScope(Box::new(scope)));
                }
            }

            if t._type == TokenType::If {
                self.consume(1);
                if let Some(t_2) = self.peek(0) {
                    if t_2._type == TokenType::OpenParent {
                        self.consume(1);
                        if let Some(node_expr) = self.parse_expr(0, Type::Bool) {
                            if node_expr.get_type() != Type::Bool {
                                eprintln!("Error: Expected boolean expression");
                                exit(-1);
                            }
                            if let Some(t_3) = self.peek(0) {
                                if t_3._type == TokenType::CloseParent {
                                    self.consume(1);
                                    if let Some(scope) = self.parse_scope(type_.clone()) {
                                        node = Some(NodeStmt::NodeStmtIf(NodeStmtIf {
                                            expr: node_expr,
                                            scope: Box::new(scope),
                                        }));
                                        return node;
                                    } else {
                                        eprintln!("Error: Expected scope");
                                        exit(-1);
                                    }
                                }
                            } else {
                                eprintln!("Error: Expected expression");
                                exit(-1);
                            }
                        } else {
                            eprintln!("Error: Expected '('");
                            exit(-1);
                        }
                    } else {
                        eprintln!("Error: Expected '('");
                        exit(-1);
                    }
                }
            }
            if t._type == TokenType::Else {
                self.consume(1);
                //Get if the last node was an if, if not then error
                if  self.prog.stmts.len()  == 0  {
                    eprintln!("Error: Expected 'if'");
                        exit(-1);
                }
                let last_tokens = &self.prog.stmts[self.prog.stmts.len() - 1];
                match last_tokens {
                    NodeStmt::NodeStmtIf(_) => {}
                    NodeStmt::NodeStmtElseIf(_) => {}
                    _ => {

                        eprintln!("Error: Expected 'if'");
                        exit(-1);
                    }
                }
                //test is passed check if it is an else if
                if let Some(t_2) = self.peek(0) {
                    if t_2._type == TokenType::If {
                        if let Some(mut node_expr) = self.parse_stmt(type_.clone()) {
                            match node_expr {
                                NodeStmt::NodeStmtIf(node_if) => {
                                    node_expr = NodeStmt::NodeStmtElseIf(NodeStmtElseIf {
                                        expr: node_if.expr,
                                        scope: Box::new(*node_if.scope),
                                    });
                                    return Some(node_expr);
                                    
                                }
                                _ => {
                                    eprintln!("Error: Expected 'if'");
                                    exit(-1);
                                }
                            }
                           
                        } else {
                            eprintln!("Error: Expected 'if'");
                            exit(-1);
                        }
                    }
                }
                //test is passed check if it is an else
                if let Some(t_2) = self.peek(0) {
                    if t_2._type == TokenType::OpenCurly {
                        let scope = self.parse_scope(type_.clone());
                        if let Some(scope) = scope {
                            node = Some(NodeStmt::NodeStmtElse(NodeStmtElse {
                                scope: Box::new(scope),
                            }));
                            
                        }
                        if let Some(t_3) = self.peek(0) {
                            if t_3._type == TokenType::CloseCurly {
                                self.consume(1);
                                return node;
                            } else {
                                eprintln!("Error: Expected '}}' found {:?}", t_3);
                                exit(-1);
                            }
                        } 
                    }
                } else {
                    eprintln!("Error: Expected 'if'");
                    exit(-1);
                }
            }

            if t._type == TokenType::While {
                self.consume(1);
                if let Some(t_2) = self.peek(0) {
                    if t_2._type == TokenType::OpenParent {
                        self.consume(1);
                        if let Some(node_expr) = self.parse_expr(0, Type::Bool) {
                            if node_expr.get_type() != Type::Bool {
                                eprintln!("Error: Expected boolean expression");
                                exit(-1);
                            }
                            if let Some(t_3) = self.peek(0) {
                                if t_3._type == TokenType::CloseParent {
                                    self.consume(1);

                                    if let Some(scope) = self.parse_scope(type_.clone()) {
                                        node = Some(NodeStmt::NodeStmtWhile(NodeStmtWhile {
                                            expr: node_expr,
                                            scope: Box::new(scope),
                                        }));
                                        return node;
                                    } else {
                                        eprintln!("Error: Expected scope");
                                        exit(-1);
                                    }
                                }
                            } else {
                                eprintln!("Error: Expected expression");
                                exit(-1);
                            }
                        } else {
                            eprintln!("Error: Expected '('");
                            exit(-1);
                        }
                    } else {
                        eprintln!("Error: Expected '('");
                        exit(-1);
                    }
                }
            }

            if let Some(t_2) = self.peek(1) {
                if t._type == TokenType::Exit && t_2._type == TokenType::OpenParent {
                    self.consume(1);
                    self.consume(1);

                    if let Some(node_expr) = self.parse_expr(0, type_.clone()) {
                        node = Some(NodeStmt::NodeStmtExit(NodeStmtExit { expr: node_expr }));
                    } else {
                        eprintln!("Error: Expected expression");
                        exit(-1);
                    }
                    if let Some(t) = self.peek(0) {
                        if t._type == TokenType::CloseParent {
                            self.consume(1);
                        } else {
                            eprintln!("Error: Expected ')'");
                            exit(-1);
                        }
                    } else {
                        eprintln!("Error: Expected ')'");
                        exit(-1);
                    }

                    if let Some(t) = self.peek(0) {
                        if t._type == TokenType::Semi {
                            self.consume(1);
                        } else {
                            eprintln!("Error: Expected ';'");
                            exit(-1);
                        }
                    } else {
                        eprintln!("Error: Expected ';'");
                        exit(-1);
                    }
                    return node;
                }
                if t._type == TokenType::Print && t_2._type == TokenType::OpenParent {
                    self.consume(1);
                    self.consume(1);

                    if let Some(node_expr) = self.parse_expr(0, type_.clone()) {
                        node = Some(NodeStmt::NodeStmtPrint(NodeStmtPrint { expr: node_expr }));
                    } else {
                        eprintln!("Error: Expected expression");
                        exit(-1);
                    }
                    if let Some(t) = self.peek(0) {
                        if t._type == TokenType::CloseParent {
                            self.consume(1);
                        } else {
                            eprintln!("Error: Expected ')'");
                            exit(-1);
                        }
                    } else {
                        eprintln!("Error: Expected ')'");
                        exit(-1);
                    }

                    if let Some(t) = self.peek(0) {
                        if t._type == TokenType::Semi {
                            self.consume(1);
                        } else {
                            eprintln!("Error: Expected ';'");
                            exit(-1);
                        }
                    } else {
                        eprintln!("Error: Expected ';'");
                        exit(-1);
                    }
                    return node;
                }
                if t._type == TokenType::Println && t_2._type == TokenType::OpenParent {
                    self.consume(1);
                    self.consume(1);

                    if let Some(node_expr) = self.parse_expr(0, type_.clone()) {
                        node = Some(NodeStmt::NodeStmtPrintln(NodeStmtPrintln {
                            expr: node_expr,
                        }));
                    } else {
                        eprintln!("Error: Expected expression");
                        exit(-1);
                    }
                    if let Some(t) = self.peek(0) {
                        if t._type == TokenType::CloseParent {
                            self.consume(1);
                        } else {
                            eprintln!("Error: Expected ')'");
                            exit(-1);
                        }
                    } else {
                        eprintln!("Error: Expected ')'");
                        exit(-1);
                    }

                    if let Some(t) = self.peek(0) {
                        if t._type == TokenType::Semi {
                            self.consume(1);
                        } else {
                            eprintln!("Error: Expected ';'");
                            exit(-1);
                        }
                    } else {
                        eprintln!("Error: Expected ';'");
                        exit(-1);
                    }
                    return node;
                }

                if let Some(t_3) = self.peek(2) {
                    if let Some(t_4) = self.peek(3) {
                        if let Some(t_5) = self.peek(4) {
                            if t._type == TokenType::Let
                                && t_2._type == TokenType::Ident
                                && t_3._type == TokenType::Colon
                                && (t_4._type == TokenType::Int
                                    || t_4._type == TokenType::Bool
                                    || t_4._type == TokenType::Char)
                                && t_5._type == TokenType::Eq
                            {
                                self.consume(1); // consume let
                                let ident = self.consume(1); // consume ident
                                self.consume(1); // consume colon
                                let _type = self.consume(1); // consume type
                                let var_type;
                                match _type._type {
                                    TokenType::Int => var_type = Type::Int,
                                    TokenType::Bool => var_type = Type::Bool,
                                    TokenType::Char => var_type = Type::Char,
                                    _ => {
                                        eprintln!("Error: Expected type");
                                        exit(-1);
                                    }
                                }

                                self.consume(1); // consume equal

                                if let Some(expr) = self.parse_expr(0, var_type.clone()) {
                                    self.variables_types.push((ident.clone(), var_type.clone()));
                                    node = Some(NodeStmt::NodeStmtLet(NodeStmtLet {
                                        ident: ident,
                                        expr: expr,
                                    }));
                                } else {
                                    eprintln!("Error: Expected expression");
                                    exit(-1);
                                }
                                if let Some(t) = self.peek(0) {
                                    if t._type == TokenType::Semi {
                                        self.consume(1);
                                    } else {
                                        eprintln!("Error: Expected ';'");
                                        exit(-1);
                                    }
                                } else {
                                    eprintln!("Error: Expected ';'");
                                    exit(-1);
                                }
                                return node;
                            }
                        }
                    }
                    if t._type == TokenType::Ident && t_2._type == TokenType::Eq {
                        let ident = self.consume(1); // consume ident
                        let aux = self.variables_types.clone();
                        let var_type = aux.iter().find(|x| x.0 == ident);
                        if var_type.is_none() {
                            eprintln!("Error: Variable not declared: {}", ident.value.unwrap());
                            exit(-1);
                        }
                        let var_type = var_type.unwrap();

                        self.consume(1); // consume equal

                        if let Some(expr) = self.parse_expr(0, var_type.1.clone()) {
                            if var_type.1 != expr.get_type() {
                                eprintln!(
                                    "Error: Type mismatch {:?} and {:?}",
                                    var_type.1,
                                    expr.get_type()
                                );
                                exit(-1);
                            }
                            node = Some(NodeStmt::NodeStmtAssign(NodeStmtAssign {
                                ident: ident,
                                expr: expr,
                            }));
                        } else {
                            eprintln!("Error: Expected expression");
                            exit(-1);
                        }
                        if let Some(t) = self.peek(0) {
                            if t._type == TokenType::Semi {
                                self.consume(1);
                            } else {
                                eprintln!("Error: Expected ';'");
                                exit(-1);
                            }
                        }
                    }
                }
            }
        }

        node
    }

    pub fn parse_prog(&mut self) -> Option<NodeProg> {
        while let Some(_t) = self.peek(0) {
            if let Some(stmt) = self.parse_stmt(Type::Int) {
                self.prog.stmts.push(stmt);
            } else {
                return None;
            }
        }
        Some(self.prog.clone())
    }

    fn peek(&self, ahead: usize) -> Option<Token> {
        if self.index + ahead >= self.tokens.len() {
            None
        } else {
            Some(self.tokens[self.index + ahead].clone())
        }
    }

    fn consume(&mut self, ahead: usize) -> Token {
        self.index += ahead;
        self.tokens[self.index - 1].clone()
    }
}
