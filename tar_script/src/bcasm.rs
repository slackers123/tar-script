use crate::ast;

use crate::bcvm::Val;
use crate::bcvm::BCInst;
use crate::bcvm::Type;

use std::collections::HashMap;

pub fn assemble_bc(defs: Vec<ast::AstNode>, funcs: HashMap<String, ast::AstNode>) -> (HashMap<String, (Vec<u8>, Vec<Val>)>, Vec<Val>, Option<String>) {
    let mut fns: HashMap<String, (Vec<u8>, Vec<Val>)> = HashMap::new();
    fns.insert("log".to_owned(), (vec![BCInst::PRINT], vec![]));
    fns.insert("push".to_owned(), (vec![BCInst::STORE_LOCAL_VAL, 0, BCInst::STORE_LOCAL_VAL, 1, BCInst::LOAD_LOCAL_VAL, 0, BCInst::LOAD_LOCAL_VAL, 1, BCInst::PUSH_TO_ARR], vec![]));
    fns.insert("get_len".to_owned(), (vec![BCInst::LOAD_ARR_LEN], vec![]));
    fns.insert("get_at".to_owned(), (vec![BCInst::STORE_LOCAL_VAL, 0, BCInst::STORE_LOCAL_VAL, 1, BCInst::LOAD_LOCAL_VAL, 0, BCInst::LOAD_LOCAL_VAL, 1, BCInst::GET_ARR_AT], vec![]));
    let mut entry = None;
    for d in defs { // TODO: handle imports
        match d {
            ast::AstNode::Definition{target, value} => {
                if target == "entry" {
                    entry = Some(value);
                }
            }

            ast::AstNode::Import(path) => {
                if path[0] == "std" {
                    
                }
            }

            _=> {println!("error: not a definition or a import")}
        }
    }

    for f in funcs {
        let (name, func) = f;
        let mut bc: Vec<u8> = vec![];
        let mut consts = vec![];
        let mut temp_vars: HashMap<String, u8> = HashMap::new();
        if let ast::AstNode::FuncDef{ident, args, ret_ty: _, block} = func {
            assert_eq!(ident, name);
            if args.is_some() {
                let mut i = 0;
                let gs = args.unwrap();
                while i < gs.len() {
                    bc.push(BCInst::STORE_LOCAL_VAL);
                    bc.push(i as u8);
                    if let ast::AstNode::Arg{ident, ty: _} = gs[i].clone() {
                        temp_vars.insert(ident, i as u8);
                    }
                    i+=1;
                }
            }
            asm_block(&mut bc, &mut consts, &mut temp_vars, block);
        }

        fns.insert(name, (bc, consts));
    }

    return (fns, vec![], entry);
}

fn push_to_consts(consts: &mut Vec<Val>, val: Val) -> usize {
    let index = consts.iter().position(|r| *r == val);
    if index.is_some() {
        return index.unwrap();
    }
    let idx = consts.len();
    consts.push(val);
    return idx;
}

fn asm_block(bc: &mut Vec<u8>, consts: &mut Vec<Val>, temp_vars: &mut HashMap<String, u8>, block: Vec<ast::AstNode>) -> usize {
    let mut len = 0;
    for b in block {
        match b {
            ast::AstNode::FuncCall {ident, has_avp, args} => {
                len += asm_func_call(bc, consts, &temp_vars, ident, args, has_avp);
            }

            ast::AstNode::IfStmt{condition, block, else_if_stmt, else_stmt} => {
                match *condition {
                    ast::AstNode::Bool(b) => {
                        let idx = push_to_consts(consts, Val::Bool(b));
                        bc.push(BCInst::LOAD_CONST);
                        bc.push(idx as u8);
                        len += 2;
                    }

                    ast::AstNode::BoolOp{op, lhs, rhs} => {
                        len += asm_bool_op(bc, consts, &temp_vars, op, *lhs, *rhs);
                    }

                    ast::AstNode::FuncCall{ident, has_avp, args} => {
                        len += asm_func_call(bc, consts, &temp_vars, ident, args, has_avp);
                    }

                    _ => {panic!("not supported: {:?}", *condition)}
                }
                let mut nbc = bc.clone();
                asm_block(&mut nbc, &mut consts.clone(), temp_vars, block.clone());
                let diff = nbc.len() as u64 - bc.len() as u64 + 1;
                bc.push(BCInst::JUMP_IF_FALSE);
                len += 1;
                len += asm_jmp_dist(bc, diff, true, false);
                len += asm_block(bc, consts, temp_vars, block);

                if else_if_stmt.is_some() {todo!("implement else if and else")}
                if else_stmt.is_some() {todo!("implement else if and else")}
            }

            ast::AstNode::Declaration{ty: _, ident, val} => {
                let idx = temp_vars.len() as u8;
                temp_vars.insert(ident, idx);
                if ast::is_const(*val.clone()) {
                    println!("calling load_val from asm_block decl");
                    len += load_val(bc, consts, temp_vars, *val);
                }
                else {
                    len += asm_expr(bc, consts, temp_vars, *val);
                }
                bc.push(BCInst::STORE_LOCAL_VAL);
                bc.push(idx);
                len += 2;
            }

            ast::AstNode::ValAssign{ident, val} => {
                println!("calling load_val from asm_block val assign");
                len += load_val(bc, consts, temp_vars, *val);

                bc.push(BCInst::STORE_LOCAL_VAL);
                bc.push(*temp_vars.get(&ident).unwrap());
                len += 2;

            }

            ast::AstNode::WhileLoop{condition, block} => {
                let l1 = len;
                match *condition {
                    ast::AstNode::Bool(b) => {
                        let idx = push_to_consts(consts, Val::Bool(b));
                        bc.push(BCInst::LOAD_CONST);
                        bc.push(idx as u8);
                        len += 2;
                    }

                    ast::AstNode::BoolOp{op, lhs, rhs} => {
                        len += asm_bool_op(bc, consts, &temp_vars, op, *lhs, *rhs);
                    }

                    ast::AstNode::FuncCall{ident, has_avp, args} => {
                        len += asm_func_call(bc, consts, &temp_vars, ident, args, has_avp);
                    }

                    _ => {panic!("not supported: {:?}", *condition)}
                }

                let len_tmp = len - l1;

                let l = asm_block(&mut bc.clone(), &mut consts.clone(), &mut temp_vars.clone(), block.clone()) as u64;
                bc.push(BCInst::JUMP_IF_FALSE);
                let i = bc.len(); // TODO: use the i to take of the top of the bc vec insert the jump distance and put back the bytecode (maybe make it into a new func)
                bc.push(0);
                bc.get
                len += asm_jmp_dist(bc, l, true, true);
                len += asm_block(bc, consts, temp_vars, block);
                bc.push(BCInst::JUMP);
                len += asm_jmp_dist(bc, l + len_tmp as u64, false, true);
                len += 2;
            }

            ast::AstNode::ReturnStmt(ret) => {
                len += asm_expr(bc, consts, temp_vars, *ret);
            }

            _ => {panic!("not supported: {:?}", b)}
        }
    }
    return len;
}

fn asm_jmp_dist(bc: &mut Vec<u8>, diff: u64, go_fwd: bool, in_while: bool) -> usize {
    let mut len = 0;
    let mut diff = diff;
    let fwd = (!go_fwd as u8)<<4;
    if diff & 4294967295 == diff {
        if diff as u32 & 65535 == diff as u32 {
            if diff as u16 & 255 == diff as u16 {
                if in_while {diff+=3}
                bc.push(fwd | 0x01);
                bc.push(diff as u8);
                len += 2;
            }
            else {
                if in_while {diff+=4}
                bc.push(fwd | 0x02);
                bc.push((diff>>8) as u8);
                bc.push(diff as u8);
                len += 3;
            }
        }
        else {
            if in_while {diff+=6}
            bc.push(fwd | 0x04);
            bc.push((diff>>8) as u8);
            bc.push((diff>>16) as u8);
            bc.push((diff>>24) as u8);
            bc.push(diff as u8);
            len += 5;
        }
    }
    else {
        if in_while {diff+=10}
        bc.push(fwd | 0x08);
        bc.push((diff>>8) as u8);
        bc.push((diff>>16) as u8);
        bc.push((diff>>24) as u8);
        bc.push((diff>>32) as u8);
        bc.push((diff>>40) as u8);
        bc.push((diff>>48) as u8);
        bc.push((diff>>56) as u8);
        bc.push(diff as u8);
        len += 9;
    }

    return len;
}

fn asm_func_call(bc: &mut Vec<u8>, consts: &mut Vec<Val>, temp_vars: &HashMap<String, u8>, ident: String, args: Vec<ast::AstNode>, has_avp: bool) -> usize {
    let mut len = 0;
    let mut i = args.len() as i32-1;
    let mut var = 0;
    while i >= 0{
        println!("calling load_val from asm_func_call");
        len += load_val(bc, consts, temp_vars, args[i as usize].clone());
        if i == 0 {
            var = bc[bc.len()-1];
        }
        i-=1;
    }
    let idx = push_to_consts(consts, Val::String(ident.clone()));
    bc.push(BCInst::LOAD_CONST);
    bc.push(idx as u8);
    bc.push(BCInst::CALL_FUNC);
    len += 3;

    if has_avp {
        if args.len() > 0 {
            bc.push(BCInst::STORE_LOCAL_VAL);
            bc.push(var);
            len += 2;
        }
        else {
            panic!("cannot perform automatic value propagation on functions which take no inputs");
        }
    }

    return len;
}

fn asm_expr(bc: &mut Vec<u8>, consts: &mut Vec<Val>, temp_vars: &HashMap<String, u8>, expr: ast::AstNode) -> usize{
    let mut len = 0;
    match expr {
        ast::AstNode::Ident(v) => {
            bc.push(BCInst::LOAD_LOCAL_VAL);
            bc.push(*temp_vars.get(&v).unwrap());
            len += 2;
        }

        ast::AstNode::Integer(i) => {
            let idx = push_to_consts(consts, Val::Int(i));
            bc.push(BCInst::LOAD_CONST);
            bc.push(idx as u8);
            len += 2;
        }

        ast::AstNode::Float(f) => {
            let idx = push_to_consts(consts, Val::Float(f));
            bc.push(BCInst::LOAD_CONST);
            bc.push(idx as u8);
            len += 2;
        }

        ast::AstNode::String(s) => {
            let idx = push_to_consts(consts, Val::String(s));
            bc.push(BCInst::LOAD_CONST);
            bc.push(idx as u8);
            len += 2;
        }

        ast::AstNode::BinOp{op, lhs, rhs} => {
            len += asm_bin_op(bc, consts, temp_vars, op, *lhs, *rhs);
        }

        ast::AstNode::FuncCall{ident, has_avp, args} => {
            len += asm_func_call(bc, consts, temp_vars, ident, args, has_avp);
        }

        ast::AstNode::Array{ty, arr} => {
            println!("calling load_val from asm_expr");
            len += load_val(bc, consts, temp_vars, ast::AstNode::Array{ty, arr});
        }

        _ => {panic!("not supported: {:?}", expr);}
    }
    return len;
}

fn asm_bin_op(bc: &mut Vec<u8>, consts: &mut Vec<Val>, temp_vars: &HashMap<String, u8>, op: ast::BinOp, lhs: ast::AstNode, rhs: ast::AstNode) -> usize {
    let mut len = 0;
    // println!("bc at {:?}: {:?}", op, bc);
    len += asm_expr(bc, consts, temp_vars, lhs);
    len += asm_expr(bc, consts, temp_vars, rhs);
    // println!("bc: {:?}", bc);
    match op {
        ast::BinOp::Plus => {
            bc.push(BCInst::ADD);
        }
        ast::BinOp::Minus => {
            bc.push(BCInst::SUB);
        }
        ast::BinOp::Mul => {
            bc.push(BCInst::MUL);
        }
        ast::BinOp::Div => {
            bc.push(BCInst::DIV);
        }
    }
    len += 1;

    return len;
}

fn asm_bool_op(bc: &mut Vec<u8>, consts: &mut Vec<Val>, temp_vars: &HashMap<String, u8>, op: ast::BoolOp, lhs: ast::AstNode, rhs: ast::AstNode) -> usize {
    let mut len = 0;
    len += asm_expr(bc, consts, temp_vars, lhs);
    len += asm_expr(bc, consts, temp_vars, rhs);
    match op {
        ast::BoolOp::Equal => {
            bc.push(BCInst::EQUAL);
        }
        ast::BoolOp::GreaterThan => {
            bc.push(BCInst::GREATER_THAN);
        }
        ast::BoolOp::LessThan => {
            bc.push(BCInst::LESS_THAN);
        }
        ast::BoolOp::GreaterThanEqual => {
            bc.push(BCInst::GREATER_THAN_EQUAL);
        }
        ast::BoolOp::LessThanEqual => {
            bc.push(BCInst::LESS_THAN_EQUAL);
        }
        _ => {todo!("implement all boolean operations in the vm")}
    }
    len += 1;
    return len;
}

fn load_val(bc: &mut Vec<u8>, consts: &mut Vec<Val>, temp_vars: &HashMap<String, u8>, ast: ast::AstNode) -> usize {
    let mut len = 0;
    match ast {
        ast::AstNode::Integer(i) => {
            let idx = push_to_consts(consts, Val::Int(i));

            bc.push(BCInst::LOAD_CONST);
            bc.push(idx as u8);
            return 2;
        }
        ast::AstNode::String(s) => {
            let idx = push_to_consts(consts, Val::String(s));

            bc.push(BCInst::LOAD_CONST);
            bc.push(idx as u8);
            return 2;
        }
        ast::AstNode::Bool(b) => {
            let idx = push_to_consts(consts, Val::Bool(b));

            bc.push(BCInst::LOAD_CONST);
            bc.push(idx as u8);
            return 2;
        }
        ast::AstNode::Array{ty, arr} => {
            let mut i = arr.len() as  i32 -1;
            while i >= 0 {
                println!("calling load_val from load_val");
                len += load_val(bc, consts, temp_vars, arr[i as usize].clone());
                i-= 1;
            }
            let idx = push_to_consts(consts, Val::String(ty.clone()));
            bc.push(BCInst::LOAD_CONST);
            bc.push(idx as u8);
            let idx = push_to_consts(consts, Val::Int(arr.len() as i32));
            bc.push(BCInst::LOAD_CONST);
            bc.push(idx as u8);

            bc.push(BCInst::MAKE_ARR);
            
            return len + 5;
        }
        ast::AstNode::Ident(i) => {
            len += asm_expr(bc, consts, temp_vars, ast::AstNode::Ident(i));
            return len;
        }

        ast::AstNode::FuncCall{ident, has_avp, args} => {
            len += asm_func_call(bc, consts, temp_vars, ident, args, has_avp);
            return len;
        }

        ast::AstNode::BinOp{op, lhs, rhs} => {
            if format!("{:?}", op) == "Div" {
                println!("calling bin op from load_val: {:?}", op);
            }
            len += asm_bin_op(bc, consts, temp_vars, op, *lhs, *rhs);
            return len;
        }

        _ => {panic!("not a valid value: {:?}", ast)}
    }
}