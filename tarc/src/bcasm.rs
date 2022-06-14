use crate::ast;

use crate::bcvm;
use crate::bcvm::Val;
use crate::bcvm::BCInst;
use crate::bcvm::Type;
use crate::bcvm::type_s::*;

use crate::ast::AstNode;

use std::collections::HashMap;

struct Vairable {
    ty: Type,
    last_usage_x: usize,
    last_usage_y: usize,
}

struct Registers {
    ireg:  usize,
    lreg:  usize,
    freg:  usize,
    dreg:  usize,
    sreg:  usize,
    breg:  usize,
    creg:  usize,
    iareg: usize,
    lareg: usize,
    fareg: usize,
    dareg: usize,
    sareg: usize,
    bareg: usize,
    careg: usize,
}

impl Default for Registers {
    fn default() -> Self {
        Registers {
            ireg:  0,
            lreg:  0,
            freg:  0,
            dreg:  0,
            sreg:  0,
            breg:  0,
            creg:  0,
            iareg: 0,
            lareg: 0,
            fareg: 0,
            dareg: 0,
            sareg: 0,
            bareg: 0,
            careg: 0,
        }
    }
}

pub fn assemble_bc(defs: Vec<AstNode>, funcs: HashMap<String, AstNode>) -> (HashMap<String, (Vec<u8>, Vec<Val>)>, Option<String>) {
    let mut fns: HashMap<String, (Vec<u8>, Vec<Val>)> = HashMap::new();
    fns.insert("log".to_owned(), (vec![BCInst::LDA, 0x49, 0x24, 0, 0, BCInst::PRT, 0x49, 0x24], vec![]));
    let mut entry = None;
    for d in defs { // TODO: handle imports
        match d {
            AstNode::Definition{target, value} => {
                if target == "entry" {
                    entry = Some(value);
                }
            }

            AstNode::Import(path) => {
                if path[0] == "std" {
                    
                }
            }

            _=> {println!("error: not a definition or a import")}
        }
    }

    let mut ret_tys = HashMap::new();

    for f in funcs.clone() {
        if let AstNode::FuncDef{ident, args: _, ret_ty, block: _} = f.1 {
            ret_tys.insert(f.0.clone(), ret_ty);
        }
    }

    for f in funcs {
        let name = f.0.clone();
        // let vars = first_pass(f.clone());

        let bc_consts = building_pass(f, ret_tys.clone());

        fns.insert(name, bc_consts);
    }

    return (fns, entry);
}

fn building_pass(f: (String, AstNode), ret_tys: HashMap<String, Option<String>>) -> (Vec<u8>, Vec<Val>) {
    let mut bc = vec![];
    let mut consts = vec![];

    let (name, func) = f;

    let mut used_regs: HashMap<String, u16> = HashMap::new();
    let mut regs = Registers::default();

    if let AstNode::FuncDef{ident, args, ret_ty, block} = func {
        assert_eq!(ident, name);
        if args.is_some() {
            let mut i = 0;
            for a in args.unwrap() {
                if let AstNode::Arg{ident, ty} = a {
                    bc.push(BCInst::LDA);
                    let r = get_reg(&mut used_regs, &mut regs, get_as_type(ty), ident);
                    push_16bit(&mut bc, r);
                    push_16bit(&mut bc, i as u16);
                    i+=1;
                }
            }
        }

        if ret_ty.is_some() {
            println!("handle return type");
        }

        build_block(&mut bc, &mut used_regs, &mut regs, &mut consts, &ret_tys, block);
    }

    return (bc, consts)
}

fn build_block(bc: &mut Vec<u8>, used: &mut HashMap<String, u16>, regs: &mut Registers, consts: &mut Vec<Val>, ret_tys: &HashMap<String, Option<String>>, block: Vec<AstNode>) {
    for b in block {
        match b {
            AstNode::FuncCall{ident, args, has_avp} => {
                for arg in args {
                    let r = build_expr(bc, used, regs, consts, ret_tys, arg);
                    bc.push(BCInst::PA);
                    push_16bit(bc, r);
                }

                let r = get_new_const(bc, used, regs, consts, Val::String(ident), Type::String);
                
                bc.push(BCInst::CF);
                push_16bit(bc, r);

                if has_avp {
                    todo!("handle avp");
                }
            }

            AstNode::Declaration{ty, ident, val} => {
                let t = get_as_type(ty);

                match t {
                    Type::Int => {
                        if let AstNode::Integer(i) = *val {
                            get_new_const_named(bc, used, regs, consts, Val::Int(i), t, ident);
                        }
                    }

                    Type::Long => {
                        todo!("implement missing types in ast");
                    }

                    Type::Float => {
                        if let AstNode::Float(i) = *val {
                            get_new_const_named(bc, used, regs, consts, Val::Float(i as f32), t, ident);
                        }
                    }

                    Type::Double => {
                        todo!("implement missing types in ast");
                    }

                    Type::String => {
                        if let AstNode::String(i) = *val {
                            get_new_const_named(bc, used, regs, consts, Val::String(i), t, ident);
                        }
                    }

                    Type::Bool => {
                        if let AstNode::Bool(i) = *val {
                            get_new_const_named(bc, used, regs, consts, Val::Bool(i), t, ident);
                        }
                    }

                    Type::Char => {
                        todo!("implement missing types in ast");
                    }

                    Type::IntArr => {
                        todo!("implement missing types in ast");
                    }

                    Type::LongArr => {
                        todo!("implement missing types in ast");
                    }
                    
                    Type::FloatArr => {
                        todo!("implement missing types in ast");
                    }

                    Type::DoubleArr => {
                        todo!("implement missing types in ast");
                    }

                    Type::StringArr => {
                        todo!("implement missing types in ast");
                    }

                    Type::CharArr => {
                        todo!("implement missing types in ast");
                    }
                    
                    Type::BoolArr => {
                        todo!("implement missing types in ast");
                    }
                }
            }

            AstNode::ReturnStmt(ident) => {
                let r = build_expr(bc, used, regs, consts, ret_tys, *ident);
                bc.push(BCInst::RET);
                push_16bit(bc, r);
            }

            AstNode::IfStmt{condition, block, else_if_stmt, else_stmt} => {
                let r = build_condition(bc, used, regs, consts, ret_tys, *condition);
                
                let i = bc.len();
                bc.push(BCInst::JIF);
                push_16bit(bc, 0);
                push_16bit(bc, 0);

                build_block(bc, used, regs, consts, ret_tys, block);

                let l = bc.len();

                let d = (l - i - 5) as u16;

                bc[i+1] = (r >> 8) as u8;
                bc[i+2] = r as u8;
                bc[i+3] = (d >> 8) as u8;
                bc[i+4] = d as u8;
            }

            AstNode::WhileLoop{condition, block} => {
                let i1 = bc.len();
                let r = build_condition(bc, used, regs, consts, ret_tys, *condition);

                let i3 = bc.len();
                bc.push(BCInst::JIF);
                push_16bit(bc, 0);
                push_16bit(bc, 0);

                let i2 = bc.len();
                build_block(bc, used, regs, consts, ret_tys, block);

                let l1 = bc.len();

                let d1 = l1 - i1;

                let d1 = -(d1 as i16) -5;

                bc.push(BCInst::JMP);
                bc.push((d1>>8) as u8);
                bc.push(d1 as u8);

                let l2 = bc.len();

                let d2 = (l2 - i2) as i16;

                bc[i3+1] = (r>>8) as u8;
                bc[i3+2] = r as u8;
                bc[i3+3] = (d2>>8) as u8;
                bc[i3+4] = d2 as u8;
            }

            AstNode::ValAssign{ident, val} => {
                let rs = build_expr(bc, used, regs, consts, ret_tys, *val);

                let tr = *used.get(&ident).unwrap();

                bc.push(BCInst::MV);
                push_16bit(bc, tr);
                push_16bit(bc, rs);
            }

            _ => {
                panic!("unknown block member: {:?}", b);
            }
        }
    }
}

fn build_condition(bc: &mut Vec<u8>, used: &mut HashMap<String, u16>, regs: &mut Registers, consts: &mut Vec<Val>, ret_tys: &HashMap<String, Option<String>>, cond: AstNode) -> u16 {
    match cond {
        AstNode::Bool(b) => {
            get_new_const(bc, used, regs, consts, Val::Bool(b), Type::Bool)
        }

        AstNode::BoolOp{op, lhs, rhs} => {
            let lr = build_expr(bc, used, regs, consts, ret_tys, *lhs);
            let rr = build_expr(bc, used, regs, consts, ret_tys, *rhs);
            let mut inv = false;
            match op {
                ast::BoolOp::Equal => {
                    bc.push(BCInst::EQL);
                }
                ast::BoolOp::NotEqual => {
                    bc.push(BCInst::EQL);
                    inv = true;
                }
                ast::BoolOp::GreaterThan => {
                    bc.push(BCInst::GT);
                }
                ast::BoolOp::LessThan => {
                    bc.push(BCInst::LT);
                }
                ast::BoolOp::GreaterThanEqual => {
                    bc.push(BCInst::GTE);
                }
                ast::BoolOp::LessThanEqual => {
                    bc.push(BCInst::LTE);
                }
            }

            let res = get_reg(used, regs, Type::Bool, "cond_res".to_owned());
            push_16bit(bc, res);
            push_16bit(bc, lr);
            push_16bit(bc, rr);

            if inv {
                bc.push(BCInst::NOT);
                push_16bit(bc, res);
                push_16bit(bc, res);
            }

            return res;
        }

        _ => {
            panic!("invalid condition: {:?}", cond);
        }
    }
}

fn build_expr(bc: &mut Vec<u8>, used: &mut HashMap<String, u16>, regs: &mut Registers, consts: &mut Vec<Val>, ret_tys: &HashMap<String, Option<String>>, expr: AstNode) -> u16 {
    match expr {
        AstNode::String(s) => {
            get_new_const(bc, used, regs, consts, Val::String(s), Type::String)
        }

        AstNode::Integer(i) => {
            get_new_const(bc, used, regs, consts, Val::Int(i), Type::Int)
        }

        AstNode::Ident(i) => {
            get_reg_of_var(used, i)
        }

        AstNode::BinOp{op, lhs, rhs} => {
            let rl = build_expr(bc, used, regs, consts, ret_tys, *lhs);
            let rr = build_expr(bc, used, regs, consts, ret_tys, *rhs);

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
            let nr = get_reg(used, regs, bcvm::get_type(rl), "add_res".to_owned());
            push_16bit(bc, nr);
            push_16bit(bc, rl);
            push_16bit(bc, rr);

            return nr;
        }

        AstNode::FuncCall{ident, has_avp, args} => {
            for arg in args {
                let r = build_expr(bc, used, regs, consts, ret_tys, arg);
                bc.push(BCInst::PA);
                push_16bit(bc, r);
            }

            let r = get_new_const(bc, used, regs, consts, Val::String(ident.clone()), Type::String);
            
            bc.push(BCInst::CF);
            push_16bit(bc, r);

            bc.push(BCInst::CR);

            let rty = get_as_type(ret_tys.get(&ident).expect("function does not have a return type").as_ref().unwrap().clone());

            let r = get_reg(used, regs, rty, ident);
            push_16bit(bc, r);
            return r;
        }

        AstNode::Conv{expr, target} => {
            let r = build_expr(bc, used, regs, consts, ret_tys, *expr);

            let ty = get_as_type(target);

            let rt = get_reg(used, regs, ty, "conv_temp".to_owned());

            bc.push(BCInst::COV);
            push_16bit(bc, rt);
            push_16bit(bc, r);

            return rt;
        }

        _ => {
            panic!("invalid expr: {:?}", expr)
        }
    }
}

fn get_reg_of_var(used: &mut HashMap<String, u16>, name: String) -> u16 {
    let r = used.get(&name);

    *r.expect(format!("variable {} not found", name).as_str())
}

fn get_new_const(bc: &mut Vec<u8>, used: &mut HashMap<String, u16>, regs: &mut Registers, consts: &mut Vec<Val>, con: Val, ty: Type) -> u16 {
    let i = consts.len();
    consts.push(con);

    bc.push(BCInst::LC);
    let r = get_reg(used, regs, ty.clone(), format!("{:?}", ty) + "_const");
    push_16bit(bc, r);
    push_16bit(bc, i as u16);
    return r;
}

fn get_new_const_named(bc: &mut Vec<u8>, used: &mut HashMap<String, u16>, regs: &mut Registers, consts: &mut Vec<Val>, con: Val, ty: Type, name: String) -> u16 {
    let i = consts.len();
    consts.push(con);

    bc.push(BCInst::LC);
    let r = get_reg(used, regs, ty.clone(), name);
    push_16bit(bc, r);
    push_16bit(bc, i as u16);
    return r;
}

fn push_16bit(bc: &mut Vec<u8>, b: u16) {
    bc.push((b >> 8) as u8);
    bc.push(b as u8);
}

fn get_reg(used: &mut HashMap<String, u16>, regs: &mut Registers, ty: Type, name: String) -> u16 {
    let existing = used.get(&name);

    if existing.is_some() {
        return *existing.unwrap();
    }
    
    match ty {
        Type::Int => {
            let i = get_as_global(regs.ireg as u16, ty);
            regs.ireg+=1;

            used.insert(name, i as u16);

            return i;
        }

        Type::Long => {
            let i = get_as_global(regs.lreg as u16, ty);
            regs.lreg+=1;
            
            used.insert(name, i as u16);

            return i;
        }

        Type::Float => {
            let i = get_as_global(regs.freg as u16, ty);
            regs.freg+=1;
            
            used.insert(name, i as u16);

            return i;
        }

        Type::Double => {
            let i = get_as_global(regs.dreg as u16, ty);
            regs.dreg+=1;
            
            used.insert(name, i as u16);

            return i;
        }

        Type::String => {
            let i = get_as_global(regs.sreg as u16, ty);
            regs.sreg+=1;
            
            used.insert(name, i as u16);

            return i;
        }

        Type::Bool => {
            let i = get_as_global(regs.breg as u16, ty);
            regs.breg+=1;
            
            used.insert(name, i as u16);

            return i;
        }

        Type::Char => {
            let i = get_as_global(regs.creg as u16, ty);
            regs.creg+=1;
            
            used.insert(name, i as u16);

            return i;
        }

        Type::IntArr => {
            let i = get_as_global(regs.iareg as u16, ty);
            regs.iareg+=1;
            
            used.insert(name, i as u16);

            return i;
        }

        Type::LongArr => {
            let i = get_as_global(regs.lareg as u16, ty);
            regs.lareg+=1;
            
            used.insert(name, i as u16);

            return i;
        }

        Type::FloatArr => {
            let i = get_as_global(regs.fareg as u16, ty);
            regs.fareg+=1;
            
            used.insert(name, i as u16);

            return i;
        }

        Type::DoubleArr => {
            let i = get_as_global(regs.dareg as u16, ty);
            regs.dareg+=1;
            
            used.insert(name, i as u16);

            return i;
        }

        Type::StringArr => {
            let i = get_as_global(regs.sareg as u16, ty);
            regs.sareg+=1;
            
            used.insert(name, i as u16);

            return i;
        }

        Type::BoolArr => {
            let i = get_as_global(regs.bareg as u16, ty);
            regs.bareg+=1;
            
            used.insert(name, i as u16);

            return i;
        }

        Type::CharArr => {
            let i = get_as_global(regs.careg as u16, ty);
            regs.careg+=1;
            
            used.insert(name, i as u16);

            return i;
        }
    }
}

fn get_as_global(r: u16, ty: Type) -> u16 {
    match ty {
        Type::Int => {
            r
        }

        Type::Long => {
            r+4681
        }

        Type::Float => {
            r + 4681*2
        }

        Type::Double => {
            r + 4681*3
        }

        Type::String => {
            r + 4681*4
        }

        Type::Bool => {
            r + 4681*5
        }

        Type::Char => {
            r + 4681*6
        }

        Type::IntArr => {
            r + 4681*7
        }

        Type::LongArr => {
            r + 4681*8
        }

        Type::FloatArr => {
            r + 4681*9
        }

        Type::DoubleArr => {
            r + 4681*10
        }

        Type::StringArr => {
            r + 4681*11
        }

        Type::BoolArr => {
            r + 4681*12
        }

        Type::CharArr => {
            r + 4681*13
        }
    }
}

// fn first_pass(f: (String, AstNode)) -> HashMap<String, Vairable> {

//     let (name, func) = f;

//     let mut vars = HashMap::new();
//     let mut x = 0;
//     let mut y = 0;

//     if let AstNode::FuncDef{ident, args, ret_ty, block} = func {
//         if args.is_some() {
//             for a in args.unwrap() {
//                 if let AstNode::Arg{ident, ty} = a {
//                     update_vars_with(&mut vars, ident, ty, x, y);
//                 }
//                 y+=1;
//             }
//             x+=1;
//         }
//         y=0;

//         go_through_block(&mut vars, block, x, y);
//     }

//     return vars;
// }


fn update_vars_with(vars: &mut HashMap<String, Vairable>, name: String, ty: String, x: usize, y: usize) {
    vars.insert(name, Vairable {
        ty: get_as_type(ty),
        last_usage_x: x,
        last_usage_y: y,
    });
}

fn get_as_type(ty: String) -> Type {
    match ty.as_str() {
        "int" => {
            return Type::Int;
        }

        "long" => {
            return Type::Long;
        }

        "float" => {
            return Type::Float;
        }

        "double" => {
            return Type::Double;
        }

        "string" => {
            return Type::String;
        }

        "bool" => {
            return Type::Bool;
        }

        "char" => {
            return Type::Char;
        }

        "int[]" => {
            return Type::IntArr;
        }

        "long[]" => {
            return Type::LongArr;
        }

        "float[]" => {
            return Type::FloatArr;
        }

        "double[]" => {
            return Type::DoubleArr;
        }

        "string[]" => {
            return Type::StringArr;
        }

        "bool[]" => {
            return Type::BoolArr;
        }

        "char[]" => {
            return Type::CharArr;
        }

        _ => {
            panic!("unknown type {}", ty);
        }
    }
}