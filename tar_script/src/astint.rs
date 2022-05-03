use std::collections::HashMap;
use std::fmt::Display;

use crate::ast;

#[allow(unused)]
fn log<I: Display>(inp: I) {
    println!("{}", inp);
}

#[allow(unused)]
fn error<I: Display>(inp: I, call_stack: &Vec<String>) {
    println!("Error: {}", inp);
    println!("call_stack: {:?}", call_stack);
    panic!();
}


#[allow(unused)]
pub fn interpret(defs: Vec<ast::AstNode>, funcs: HashMap<String, ast::AstNode>) {
    // todos (display in console)

    let mut entry = String::new();
    if defs.len() > 0 {
        for d_i in defs {
            if let ast::AstNode::Definition{target, value} = d_i {
                if target == "entry" {
                    entry = value;
                }
            }
            else if let ast::AstNode::Import(value) = d_i{
                println!("TODO: import functions");
            }
            else {panic!("definition has to be 'definition' or 'import'.")}
        }

        if entry != "" {
            println!("Running entry function '{}'.", entry);

            let mut call_stack: Vec<String> = vec![];
            run_fn(entry, &funcs, &mut call_stack, vec![("int".to_owned(), ast::AstNode::Integer(10))]);
        }
        else {
            todo!("run lang as scripting lang")
        }
    }
    else {
        panic!("Why are you using the lang without imports or a main statement that makes literaly no sense")
    }
} 

// inputs is in the form of vec(type, value)
fn run_fn(name: String, funcs: &HashMap<String, ast::AstNode>, call_stack: &mut Vec<String>, inputs: Vec<(String, ast::AstNode)>) -> Option<(String, ast::AstNode)> {

    if call_stack.len() >= 100 {
        panic!("stack overflow error");
    }

    //TODO: implement user defined functions
    if name == "log".to_owned() {
        match &inputs[0].1 {

            ast::AstNode::Integer(int) => {
                log(int);
            }

            ast::AstNode::String(stri) => {
                log(stri);
            }

            _ => {
                panic!("unexpected input '{:?}'", &inputs[0].1);
            }
        }
        return None;
    }


    let func = funcs.get(&name).unwrap();
    if let ast::AstNode::FuncDef{ident, args, ret_ty, block}  = func {
        if !args.is_none() && args.clone().unwrap().len() != inputs.len() {
            error(format!("incorrect number of inputs to function: {}", ident), call_stack);
        }
        
        // k: ident v: type, value
        let mut vars: HashMap<String, (String, ast::AstNode)> = HashMap::new();

        if !args.is_none() {
            let tmp = args.clone().unwrap();
            let mut i = 0;
            for arg in tmp {
                if let ast::AstNode::Arg{ident, ty} = arg {
                    let inp = inputs[i].clone();
                    if inp.0 != ty {panic!("mismatched types expected '{}' but got '{}'", ty, inp.0)}
                    vars.insert(ident, (ty.clone(), get_val_checked(Box::new(inp.1), inp.0, funcs, call_stack, &vars)));
                }
                i+=1;
            }
        }

        return run_block(block, &mut vars, funcs, call_stack, ret_ty);
    }
    return None;
}

fn run_block(block: &Vec<ast::AstNode>, vars: &mut HashMap<String, (String, ast::AstNode)>, funcs: &HashMap<String, ast::AstNode>, call_stack: &mut Vec<String>, ret_ty: &Option<String>) -> Option<(String, ast::AstNode)> {
    
    for item in block {
        match item.clone() {
            ast::AstNode::Declaration {ty, ident, val} => {
                let v = get_val_checked(val, ty.clone(), funcs, call_stack, &vars);
                vars.insert(ident, (ty, v));
            }

            ast::AstNode::ValAssign {ident, val} => {
                let (ty, _) = vars.get(&ident).unwrap();
                let v = get_val_checked(val, ty.clone(), funcs, call_stack, &vars);
                #[allow(mutable_borrow_reservation_conflict)]
                vars.insert(ident, (ty.clone(), v));
            }

            ast::AstNode::FuncCall {ident, args} => {
                call_fn(ident, args, funcs, call_stack, &vars);
            }

            ast::AstNode::IfStmt {condition, block, else_if_stmt, else_stmt} => {
                let mut has_happend = false;
                if eval_condition(*condition, &funcs, call_stack, vars,) {
                    run_block(&block, vars, &funcs, call_stack, &ret_ty);
                    has_happend = true;
                }
                else if else_if_stmt.is_some() {
                    for else_if in else_if_stmt.unwrap() {
                        if run_else_if(&else_if, vars, &funcs, call_stack, ret_ty) {
                            has_happend = true;
                            break;
                        }
                    }
                }
                if !has_happend {
                    if else_stmt.is_some() {
                        run_block(&else_stmt.unwrap(), vars, &funcs, call_stack, &ret_ty);
                    }
                }
            }

            ast::AstNode::ReturnStmt(stmt) => {
                let ret = ret_ty.clone().unwrap();

                return Some((ret.clone(), get_val_checked(stmt, ret, funcs, call_stack, &vars)));
            }

            _ => {
                panic!("invalid part of function block: '{:?}'", item);
            }
        }
    }
    return None;
}

fn run_else_if(stmt: &ast::AstNode, vars: &mut HashMap<String, (String, ast::AstNode)>, funcs: &HashMap<String, ast::AstNode>, call_stack: &mut Vec<String>, ret_ty: &Option<String>) -> bool {
    if let ast::AstNode::ElseIf{condition, block} = stmt {
        if eval_condition(*condition.clone(), funcs, call_stack, vars) {
            run_block(block, vars, funcs, call_stack, ret_ty);
            return true;
        }
    }
    return false;
}

fn eval_condition(cond: ast::AstNode, funcs: &HashMap<String, ast::AstNode>, call_stack: &mut Vec<String>, vars: &HashMap<String, (String, ast::AstNode)>) -> bool {
    match cond {
        ast::AstNode::BoolOp{op, lhs, rhs} => {
            match op {
                ast::BoolOp::Equal => {
                    let (ls, rs) = extract_vals(lhs, rhs, funcs, call_stack, vars);
                    return ls == rs;
                }
                ast::BoolOp::NotEqual => {
                    let (ls, rs) = extract_vals(lhs, rhs, funcs, call_stack, vars);
                    return ls != rs;
                }
                ast::BoolOp::GreaterThan => {
                    let (ls, rs) = extract_vals(lhs, rhs, funcs, call_stack, vars);
                    return ls > rs;
                }
                ast::BoolOp::LessThan => {
                    let (ls, rs) = extract_vals(lhs, rhs, funcs, call_stack, vars);
                    return ls < rs;
                }
                ast::BoolOp::GreaterThanEqual => {
                    let (ls, rs) = extract_vals(lhs, rhs, funcs, call_stack, vars);
                    return ls >= rs;
                }
                ast::BoolOp::LessThanEqual => {
                    let (ls, rs) = extract_vals(lhs, rhs, funcs, call_stack, vars);
                    return ls <= rs;
                }
            }
        }

        ast::AstNode::Bool(bol) => {
            return bol;
        }

        _ => {
            panic!("not yet implemented: {:?}", cond);
        }
    }
}

fn extract_vals(lhs: Box<ast::AstNode>, rhs: Box<ast::AstNode>, funcs: &HashMap<String, ast::AstNode>, call_stack: &mut Vec<String>, vars: &HashMap<String, (String, ast::AstNode)>) -> (i32, i32) {
    let l = get_val(lhs, funcs, call_stack, vars).1;
    let r = get_val(rhs, funcs, call_stack, vars).1;

    #[allow(unused_assignments)]
    let mut ls = 0;
    #[allow(unused_assignments)]
    let mut rs = 0;

    if let ast::AstNode::Integer(int) = l {
        ls = int;
    }
    else {panic!()}
    
    if let ast::AstNode::Integer(int) = r {
        rs = int;
    }
    else {panic!()}

    return (ls, rs);
}

fn call_fn(ident: String, args: Vec<ast::AstNode>, funcs: &HashMap<String, ast::AstNode>, call_stack: &mut Vec<String>, vars: &HashMap<String, (String, ast::AstNode)>) -> Option<(String, ast::AstNode)> {
    call_stack.push(ident.clone());
    let mut ins = vec![];
    for arg in args {
        ins.push(get_val(Box::new(arg), funcs, call_stack, vars));
    }
    let res = run_fn(ident, funcs, call_stack, ins);
    call_stack.pop();
    return res;
}

fn eval_binop(op: ast::BinOp, lhs: Box<ast::AstNode>, rhs: Box<ast::AstNode>, funcs: &HashMap<String, ast::AstNode>, call_stack: &mut Vec<String>, vars: &HashMap<String, (String, ast::AstNode)> ) -> (String, ast::AstNode, ) {

    match op {
        ast::BinOp::Plus => {
            let l = get_int(lhs, funcs, call_stack, vars);
            let r = get_int(rhs, funcs, call_stack, vars);

            return ("int".to_owned(), ast::AstNode::Integer(l+r));
        }

        ast::BinOp::Minus => {
            let l = get_int(lhs, funcs, call_stack, vars);
            let r = get_int(rhs, funcs, call_stack, vars);

            return ("int".to_owned(), ast::AstNode::Integer(l-r));
        }

        ast::BinOp::Mul => {
            let l = get_int(lhs, funcs, call_stack, vars);
            let r = get_int(rhs, funcs, call_stack, vars);

            return ("int".to_owned(), ast::AstNode::Integer(l*r));
        }

        ast::BinOp::Div => {
            let l = get_int(lhs, funcs, call_stack, vars);
            let r = get_int(rhs, funcs, call_stack, vars);

            return ("int".to_owned(), ast::AstNode::Integer(l/r));
        }
    }
}

fn get_int(val: Box<ast::AstNode>, funcs: &HashMap<String, ast::AstNode>, call_stack: &mut Vec<String>, vars: &HashMap<String, (String, ast::AstNode)> ) -> i32 {
    if let ast::AstNode::Integer(int) = get_val_checked(val, "int".to_owned(), funcs, call_stack, vars) {
        return int;
    }
    error("expected int", call_stack);
    panic!();
}

fn get_val(val: Box<ast::AstNode>, funcs: &HashMap<String, ast::AstNode>, call_stack: &mut Vec<String>, vars: &HashMap<String, (String, ast::AstNode)> ) -> (String, ast::AstNode, ) {
    match *val {
        ast::AstNode::Integer(int) => {
            return ("int".to_owned(), ast::AstNode::Integer(int));
        }

        ast::AstNode::String(string) => {
            return ("string".to_owned(), ast::AstNode::String(string))
        }

        ast::AstNode::FuncCall{ident, args} => {
            return call_fn(ident, args, funcs, call_stack, vars).unwrap();
        }

        ast::AstNode::BinOp{op, lhs, rhs} => {
            return eval_binop(op, lhs, rhs, funcs, call_stack, vars);
        }

        ast::AstNode::Ident(ident) => {
            return vars.get(&ident).unwrap().clone();
        }

        _ => {
            panic!("unimplemented '{:?}'", val);
        }
    }
}

fn get_val_checked(val: Box<ast::AstNode>, ty: String, funcs: &HashMap<String, ast::AstNode>, call_stack: &mut Vec<String>, vars: &HashMap<String, (String, ast::AstNode)>) -> ast::AstNode {
    let (t, v) = get_val(val, funcs, call_stack, vars);

    if t==ty {return v;};

    panic!("Expected value '{}' but got '{}'", ty, t);
}