use std::fmt::Debug;
use std::collections::HashMap;
// use std::fs;
// use std::time::{Duration, Instant};

// use serde::{Serialize, Deserialize};
// use serde_json::Result;

// // use crate::bcasm::assemble_bc;
// use crate::ast::parse_to_ast;
// use crate::ast;
// use crate::TarParser;
// use crate::Rule;

// use pest::Parser;
// use pest::iterators::Pairs;

use crate::bcvm::type_s::*;

const MAX_RECURSION_DEPTH: u8 = 200;
#[non_exhaustive]
pub struct BCInst;

impl BCInst {
    /// Label is followed by a 16 bit label
    /// LB L
    pub const LB: u8                = 0x00;
    /// Prints out the value in the following register
    /// PRT R0
    pub const PRT: u8               = 0x01;
    /// loads a const into the provided register
    /// LC R0 C0
    pub const LC: u8                = 0x02;
    /// Adds two numbers
    /// ADD R0   R1   R2
    ///     R0 = R1 + R2
    pub const ADD: u8               = 0x03;
    /// Subtracts two numbers
    /// SUB R0   R1   R2
    ///     R0 = R1 - R2
    pub const SUB: u8               = 0x04;
    /// Multiplys two numbers
    /// MUL R0   R1   R2
    ///     R0 = R1 * R2
    pub const MUL: u8               = 0x05;  
    /// Devides two numbers
    /// DIV R0   R1   R2
    ///     R0 = R1 / R2
    pub const DIV: u8               = 0x06;
    /// checks if two values are equal
    /// EQ  R0   R1    R2
    ///     R0 = R1 == R2
    pub const EQL: u8               = 0x07;
    /// checks if one value is greater than another
    /// GT  R0   R1   R2
    ///     R0 = R1 > R2
    pub const GT: u8                = 0x08;
    /// checks if one value is less than another
    /// LT  R0   R1   R2
    ///     R0 = R1 < R2
    pub const LT: u8                = 0x09;
    /// checks if one value is greater than or equal to another
    /// GTE R0   R1    R2
    ///     R0 = R1 >= R2
    pub const GTE: u8               = 0x0a;
    /// checks if one value is less than or equal to another
    /// LTE R0   R1    R2
    ///     R0 = R1 <= R2
    pub const LTE: u8               = 0x0b; 
    /// OR's two values
    /// OR  R0   R1    R2
    ///     R0 = R1 || R2
    pub const OR: u8                = 0x0c;
    /// AND's two values
    /// AND R0   R1    R2
    ///     R0 = R1 && R2
    pub const AND: u8               = 0x0d;
    /// inverts a value
    /// NOT R0    R1
    ///     R0 = !R1
    pub const NOT: u8               = 0x0e;
    // stores a local value
    // SLV  S0   R0
    //      S0 = R0
    // pub const SLV: u8               = 0x0f; 
    // loads a local value
    // LLV  R0   S0
    //      R0 = S0
    // pub const LLV: u8               = 0x10;
    /// pushes an argument onto the argument stack
    /// PA  R0
    pub const PA: u8                = 0x0f;
    /// collects the return of the last function call (also external functions)
    /// CR  R0
    ///     R0 = return value
    pub const CR: u8                = 0x10;
    /// calls a function named in the register
    /// CF R0
    pub const CF: u8                = 0x11; 
    /// jumps by an amount specified (signed integer)
    /// JMP C0
    pub const JMP: u8               = 0x12;
    /// jumps by an amount specified (signed integer) if the register is true
    /// JIT  R0      C0
    ///   if R0 goto C0
    pub const JIT: u8               = 0x13;
    /// jumps by an amount specified (signed integer) if the register is false
    /// JIF  R0      C0
    ///  if !R0 goto C0
    pub const JIF: u8               = 0x14;
    /// inserts an a value at an index into an array
    /// IAA R0 R1    R2
    ///     R0[R1] = R2
    pub const IAA: u8               = 0x15;
    /// gets a value at an array position
    /// GAA R0   R1 R2
    ///     R0 = R1[R2]
    pub const GAA: u8               = 0x16;
    /// pushes a value onto an array
    /// PTA R0      R1
    ///     R0.push(R1)
    pub const PTA: u8               = 0x17;
    /// pops the value of the end of an array
    /// PFA R0     R1
    ///     R0.pop(R1)
    pub const PFA: u8               = 0x18;
    /// gets the length of an array
    /// GAL R0   R1
    ///     R0 = R1.len()
    pub const GAL: u8               = 0x19;
    /// returns a value from a function 
    /// RET R0
    pub const RET: u8               = 0x1a;
    /// calls an external function
    /// EXC R0
    pub const EXC: u8               = 0x1b;
    /// copies a value from one register to another
    /// MV  R0   R1
    ///     R0 = R1
    pub const MV: u8                = 0x1c;
    /// converts one value to another based on the register
    /// COV R0        R9362
    ///     R0(int) = R9362(float)
    pub const COV: u8               = 0x1d;
    /// loads the argument specified
    /// LDA R0   A0
    ///     R0 = A0
    pub const LDA: u8               = 0x1e;
}

#[derive(Clone, Debug, std::cmp::PartialEq)]
pub enum Val {
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    String(String),
    Bool(bool),
    Char(char),
    IntArr(Vec<i32>),
    LongArr(Vec<i64>),
    FloatArr(Vec<f32>),
    DoubleArr(Vec<f64>),
    StringArr(Vec<String>),
    BoolArr(Vec<bool>),
    CharArr(Vec<char>),
}

#[derive(Clone, Debug, std::cmp::PartialEq)]
pub enum Type {
    Int,
    Long,
    Float,
    Double,
    String,
    Bool,
    Char,
    IntArr,
    LongArr,
    FloatArr,
    DoubleArr,
    StringArr,
    BoolArr,
    CharArr,
}

#[derive(Debug)]
pub struct Registers {
    iregs: Vec<i32>,
    lregs: Vec<i64>,
    fregs: Vec<f32>,
    dregs: Vec<f64>,
    sregs: Vec<String>,
    bregs: Vec<bool>,
    cregs: Vec<char>,
    iaregs: Vec<Vec<i32>>,
    laregs: Vec<Vec<i64>>,
    faregs: Vec<Vec<f32>>,
    daregs: Vec<Vec<f64>>,
    saregs: Vec<Vec<String>>,
    baregs: Vec<Vec<bool>>,
    caregs: Vec<Vec<char>>,
}

impl Default for Registers {
    fn default() -> Self {
        Registers {
            iregs:  vec![],
            lregs:  vec![],
            fregs:  vec![],
            dregs:  vec![],
            sregs:  vec![],
            bregs:  vec![],
            cregs:  vec![],
            iaregs: vec![],
            laregs: vec![],
            faregs: vec![],
            daregs: vec![],
            saregs: vec![],
            baregs: vec![],
            caregs: vec![],
        }
    }
}

#[allow(unused)]
pub fn run_test() {
    let mut funcs = HashMap::new();

    let main_code: Vec<u8> = vec![2, 73, 36, 0, 0, 15, 73, 36, 2, 73, 36, 0, 1, 17, 73, 36];
    let main_consts: Vec<Val> = vec![Val::String("Hello, World!".to_owned()), Val::String("log".to_owned())];


    funcs.insert("main".to_owned(), (main_code, main_consts));

    funcs.insert("log".to_owned(), (vec![BCInst::LDA, 73, 36, 0, 0, BCInst::PRT, 73, 36], vec![]));
    run_func("main".to_owned(), &funcs, vec![], 0);
}

pub fn run_func(name: String, funcs: &HashMap<String, (Vec<u8>, Vec<Val>)>, args: Vec<Val>, d: u8) -> Option<Val> {
    let d = d+1;
    if d > MAX_RECURSION_DEPTH {
        panic!("stack overflow error");
    }

    let mut regs = Registers::default();

    let (func, consts) = funcs.get(&name).expect(format!("non existent function: {}", name).as_str());

    let mut fut_args: Vec<Val> = vec![];
    let mut funcret: Option<Val> = None;

    let mut i: usize = 0;

    while i < func.len() {
        match func[i] {
            BCInst::LB => {}

            BCInst::PRT => {
                print_reg(&regs, get_following_16bit(&mut i, &func));
            }

            BCInst::LC => {
                let r = get_following_16bit(&mut i, &func);
                let c = get_following_16bit(&mut i, &func);

                let con = consts[c as usize].clone();

                set_reg_to_val(&mut regs, r, con);
            }

            BCInst::ADD => {
                let target = get_following_16bit(&mut i, &func);
                let lhs = get_following_16bit(&mut i, &func);
                let rhs = get_following_16bit(&mut i, &func);

                add_checked(&mut regs, target, lhs, rhs);
            }

            BCInst::SUB => {
                let target = get_following_16bit(&mut i, &func);
                let lhs = get_following_16bit(&mut i, &func);
                let rhs = get_following_16bit(&mut i, &func);

                subtract_checked(&mut regs, target, lhs, rhs);
            }

            BCInst::MUL => {
                let target = get_following_16bit(&mut i, &func);
                let lhs = get_following_16bit(&mut i, &func);
                let rhs = get_following_16bit(&mut i, &func);

                multiply_checked(&mut regs, target, lhs, rhs);
            }

            BCInst::DIV => {
                let target = get_following_16bit(&mut i, &func);
                let lhs = get_following_16bit(&mut i, &func);
                let rhs = get_following_16bit(&mut i, &func);

                devide_checked(&mut regs, target, lhs, rhs);
            }

            BCInst::EQL => {
                let target = get_following_16bit(&mut i, &func);
                let lhs = get_following_16bit(&mut i, &func);
                let rhs = get_following_16bit(&mut i, &func);

                check_equal(&mut regs, target, lhs, rhs);
            }

            BCInst::GT => {
                let target = get_following_16bit(&mut i, &func);
                let lhs = get_following_16bit(&mut i, &func);
                let rhs = get_following_16bit(&mut i, &func);

                check_greater_than(&mut regs, target, lhs, rhs);
            }

            BCInst::LT => {
                let target = get_following_16bit(&mut i, &func);
                let lhs = get_following_16bit(&mut i, &func);
                let rhs = get_following_16bit(&mut i, &func);

                check_less_than(&mut regs, target, lhs, rhs);
            }

            BCInst::GTE => {
                let target = get_following_16bit(&mut i, &func);
                let lhs = get_following_16bit(&mut i, &func);
                let rhs = get_following_16bit(&mut i, &func);

                check_greater_than_equal(&mut regs, target, lhs, rhs);
            }

            BCInst::LTE => {
                let target = get_following_16bit(&mut i, &func);
                let lhs = get_following_16bit(&mut i, &func);
                let rhs = get_following_16bit(&mut i, &func);

                check_less_than_equal(&mut regs, target, lhs, rhs);
            }

            BCInst::OR => {
                let target = get_following_16bit(&mut i, &func);
                let lhs = get_following_16bit(&mut i, &func);
                let rhs = get_following_16bit(&mut i, &func);

                or_bool(&mut regs, target, lhs, rhs);
            }

            BCInst::AND => {
                let target = get_following_16bit(&mut i, &func);
                let lhs = get_following_16bit(&mut i, &func);
                let rhs = get_following_16bit(&mut i, &func);

                and_bool(&mut regs, target, lhs, rhs);
            }

            BCInst::NOT => {
                let target = get_following_16bit(&mut i, &func);
                let lhs = get_following_16bit(&mut i, &func);

                not_bool(&mut regs, target, lhs);
            }
            
            BCInst::PA => {
                let reg = get_following_16bit(&mut i, &func);

                fut_args.push(get_reg_as_val(&mut regs, reg));
            }

            BCInst::CR => {
                let target = get_following_16bit(&mut i, &func);

                set_reg_to_val(&mut regs, target, funcret.clone().expect("there function return buffer is empty"));
            }

            BCInst::CF => {
                let name_reg = get_following_16bit(&mut i, &func);

                let name = get_reg_as_val(&mut regs, name_reg);
                if let Val::String(n) = name {
                    funcret = run_func(n, funcs, fut_args.clone(), d+1);
                    fut_args = vec![]; // TODO: is this a good idea
                }
                else {
                    panic!("function name has to be string");
                }
            }

            BCInst::JMP => {
                let con = get_following_i16(&mut i , &func);
                i = (i as i16 +  con as i16) as usize;
            }

            BCInst::JIT => {
                let r = get_following_16bit(&mut i, &func);
                let c = get_following_i16(&mut i, &func);

                let case = get_val_bool(&mut regs, r);

                if case {
                    i = (i as i16 + c as i16) as usize;
                }
            }

            BCInst::JIF => {
                let r = get_following_16bit(&mut i, &func);
                let c = get_following_i16(&mut i, &func);

                let case = get_val_bool(&mut regs, r);

                if !case {
                    i = (i as i16 + c as i16) as usize;
                }
            }

            BCInst::IAA => {
                let array = get_following_16bit(&mut i, &func);
                let index = get_following_16bit(&mut i, &func);
                let value = get_following_16bit(&mut i, &func);

                let aty = get_type(array);
                let ity = get_type(index);
                let vty = get_type(value);

                if !array_type_match(aty.clone(), vty.clone()) {
                    panic!("the type of the value ({:?}) has to be the same as the array ({:?})", vty, aty);
                }

                if ity != Type::Long {
                    panic!("index into array has to be type long");
                }

                set_array_at(&mut regs, aty, array, index, value);
            }

            BCInst::GAA => {
                let target = get_following_16bit(&mut i, &func);
                let array  = get_following_16bit(&mut i, &func);
                let index  = get_following_16bit(&mut i, &func);

                let tty = get_type(target);
                let aty = get_type(array);
                let ity = get_type(index);

                if !array_type_match(aty.clone(), tty.clone()) {
                    panic!("the target register ({:?}) has to be of the same type as the array({:?})", tty, aty);
                }

                if ity != Type::Long {
                    panic!("index into array has to be type long");
                }

                get_array_at(&mut regs, aty, array, index, target);
            }

            BCInst::PTA => {
                let array = get_following_16bit(&mut i, &func);
                let value = get_following_16bit(&mut i, &func);

                let aty = get_type(array);
                let vty = get_type(value);

                if !array_type_match(aty.clone(), vty.clone()) {
                    panic!("the array ({:?}) and the value ({:?}) have to match for push operation", aty, vty);
                }

                push_to_arr(&mut regs, aty, array, value);
            }

            BCInst::PFA => {
                let target = get_following_16bit(&mut i, &func);
                let array = get_following_16bit(&mut i, &func);

                let tty = get_type(target);
                let aty = get_type(array);

                if !array_type_match(aty.clone(), tty.clone()) {
                    panic!("the target register ({:?}) has to match the array type ({:?})", tty, aty);
                }

                pop_from_arr(&mut regs, aty, target, array);
            }

            BCInst::GAL => {
                let target = get_following_16bit(&mut i, &func);
                let array = get_following_16bit(&mut i, &func);

                let tty = get_type(target);
                let aty = get_type(array);

                if tty != Type::Long {
                    panic!("array length is always given as type long");
                }

                get_array_len(&mut regs, aty, target, array);
            }

            BCInst::RET => {
                let r = get_following_16bit(&mut i, &func);

                return Some(get_reg_as_val(&mut regs, r));
            }

            BCInst::MV => {
                let target = get_following_16bit(&mut i, &func);
                let val_reg = get_following_16bit(&mut i, &func);

                let tty = get_type(target);
                let vty = get_type(val_reg);

                if tty != vty {
                    panic!("can not copy values to other register types without converting first");
                }

                move_reg(&mut regs, tty, target, val_reg);
            }

            BCInst::COV => {
                let target = get_following_16bit(&mut i, &func);
                let val_reg = get_following_16bit(&mut i, &func);

                conv_to(&mut regs, target, val_reg);
            }

            BCInst::LDA => {
                let target = get_following_16bit(&mut i, &func);
                let index = get_following_16bit(&mut i, &func);

                set_reg_to_val(&mut regs, target, args[index as usize].clone());
            }
            
            _ => {
                panic!("unknown command with signature: {}", func[i]);
            }
        }
        i+=1;
    }
    return None;
}

fn conv_to(regs: &mut Registers, target: u16, value: u16) {
    let tty = get_type(target);
    let vty = get_type(value);

    make_avail(regs, target);

    match tty {
        Type::Long => {
            if vty == Type::Int {
                regs.lregs[get_s_lreg(target)] = regs.iregs[get_s_ireg(value)] as i64;
                return;
            }
        }

        Type::Float => {
            if vty == Type::Int {
                regs.fregs[get_s_freg(target)] = regs.iregs[get_s_ireg(value)] as f32;
                return;
            }
            else if vty == Type::Long {
                regs.fregs[get_s_freg(target)] = regs.lregs[get_s_lreg(value)] as f32;
                return;
            }
        }
        Type::Double => {
            match vty {
                Type::Int => {
                    regs.dregs[get_s_dreg(target)] = regs.iregs[get_s_ireg(value)] as f64;
                    return;
                }

                Type::Long => {
                    regs.dregs[get_s_dreg(target)] = regs.lregs[get_s_lreg(value)] as f64;
                    return;
                }

                Type::Float => {
                    regs.dregs[get_s_dreg(target)] = regs.fregs[get_s_freg(value)] as f64;
                    return;
                }

                _ => {}
            }
        }
        Type::String => {
            match vty {
                Type::Int => {
                    regs.sregs[get_s_sreg(target)] = regs.iregs[get_s_ireg(value)].to_string();
                    return;
                }

                Type::Long => {
                    regs.sregs[get_s_sreg(target)] = regs.lregs[get_s_lreg(value)].to_string();
                    return;
                }

                Type::Float => {
                    regs.sregs[get_s_sreg(target)] = regs.fregs[get_s_freg(value)].to_string();
                    return;
                }

                Type::Double => {
                    regs.sregs[get_s_sreg(target)] = regs.dregs[get_s_dreg(value)].to_string();
                    return;
                }

                Type::Bool => {
                    regs.sregs[get_s_sreg(target)] = regs.bregs[get_s_breg(value)].to_string();
                    return;
                }

                Type::Char => {
                    regs.sregs[get_s_sreg(target)] = regs.cregs[get_s_creg(value)].to_string();
                    return;
                }

                _ => {}
            }
        }

        _ => {}
    }

    panic!("can not convert {:?} to {:?}", vty, tty);
}

fn move_reg(regs: &mut Registers, ty: Type, target: u16, value: u16) {
    match ty {
        Type::Int => {
            regs.iregs[get_s_ireg(target)] = regs.iregs[get_s_ireg(value)];
        }

        Type::Long => {
            regs.lregs[get_s_lreg(target)] = regs.lregs[get_s_lreg(value)];
        }

        Type::Float => {
            regs.fregs[get_s_freg(target)] = regs.fregs[get_s_freg(value)];
        }

        Type::Double => {
            regs.dregs[get_s_dreg(target)] = regs.dregs[get_s_dreg(value)];
        }

        Type::String => {
            regs.sregs[get_s_sreg(target)] = regs.sregs[get_s_sreg(value)].clone();
        }

        Type::Bool => {
            regs.bregs[get_s_breg(target)] = regs.bregs[get_s_breg(value)];
        }

        Type::Char => {
            regs.cregs[get_s_creg(target)] = regs.cregs[get_s_creg(value)];
        }
        Type::IntArr => {
            regs.iaregs[get_s_iareg(target)] = regs.iaregs[get_s_iareg(value)].clone();
        }

        Type::LongArr => {
            regs.laregs[get_s_lareg(target)] = regs.laregs[get_s_lareg(value)].clone();
        }

        Type::FloatArr => {
            regs.faregs[get_s_fareg(target)] = regs.faregs[get_s_fareg(value)].clone();
        }

        Type::DoubleArr => {
            regs.daregs[get_s_dareg(target)] = regs.daregs[get_s_dareg(value)].clone();
        }

        Type::StringArr => {
            regs.saregs[get_s_sareg(target)] = regs.saregs[get_s_sareg(value)].clone();
        }

        Type::BoolArr => {
            regs.baregs[get_s_bareg(target)] = regs.baregs[get_s_bareg(value)].clone();
        }

        Type::CharArr => {
            regs.caregs[get_s_careg(target)] = regs.caregs[get_s_careg(value)].clone();
        }
    }
}

fn get_array_len(regs: &mut Registers, aty: Type, target: u16, array: u16) {
    match aty {
        Type::IntArr => {
            regs.lregs[get_s_lreg(target)] = regs.iaregs[get_s_iareg(array) as usize].len() as i64;
        }

        Type::LongArr => {
            regs.lregs[get_s_lreg(target)] = regs.laregs[get_s_lareg(array) as usize].len() as i64;
        }

        Type::FloatArr => {
            regs.lregs[get_s_lreg(target)] = regs.faregs[get_s_fareg(array) as usize].len() as i64;
        }

        Type::DoubleArr => {
            regs.lregs[get_s_lreg(target)] = regs.daregs[get_s_dareg(array) as usize].len() as i64;
        }

        Type::StringArr => {
            regs.lregs[get_s_lreg(target)] = regs.saregs[get_s_sareg(array) as usize].len() as i64;
        }

        Type::BoolArr => {
            regs.lregs[get_s_lreg(target)] = regs.baregs[get_s_bareg(array) as usize].len() as i64;
        }
        
        Type::CharArr => {
            regs.lregs[get_s_lreg(target)] = regs.caregs[get_s_careg(array) as usize].len() as i64;
        }

        _ => {
            panic!("not a valid array type");
        }
    }
}

fn pop_from_arr(regs: &mut Registers, aty: Type, target: u16, array: u16) {
    match aty {
        Type::IntArr => {
            regs.iregs[get_s_ireg(target)] = regs.iaregs[get_s_iareg(array) as usize].pop().expect("array not long enough for pop operation");
        }

        Type::LongArr => {
            regs.lregs[get_s_lreg(target)] = regs.laregs[get_s_lareg(array) as usize].pop().expect("array not long enough for pop operation");
        }

        Type::FloatArr => {
            regs.fregs[get_s_freg(target)] = regs.faregs[get_s_fareg(array) as usize].pop().expect("array not long enough for pop operation");
        }

        Type::DoubleArr => {
            regs.dregs[get_s_dreg(target)] = regs.daregs[get_s_dareg(array) as usize].pop().expect("array not long enough for pop operation");
        }

        Type::StringArr => {
            regs.sregs[get_s_sreg(target)] = regs.saregs[get_s_sareg(array) as usize].pop().expect("array not long enough for pop operation");
        }

        Type::BoolArr => {
            regs.bregs[get_s_breg(target)] = regs.baregs[get_s_bareg(array) as usize].pop().expect("array not long enough for pop operation");
        }
        
        Type::CharArr => {
            regs.cregs[get_s_creg(target)] = regs.caregs[get_s_careg(array) as usize].pop().expect("array not long enough for pop operation");
        }

        _ => {
            panic!("not a valid array type");
        }
    }
}

fn push_to_arr(regs: &mut Registers, aty: Type, array: u16, value: u16) {
    match aty {
        Type::IntArr => {
            regs.iaregs[get_s_iareg(array) as usize].push(regs.iregs[get_s_ireg(value)]);
        }

        Type::LongArr => {
            regs.laregs[get_s_lareg(array) as usize].push(regs.lregs[get_s_lreg(value)]);
        }

        Type::FloatArr => {
            regs.faregs[get_s_fareg(array) as usize].push(regs.fregs[get_s_freg(value)]);
        }

        Type::DoubleArr => {
            regs.daregs[get_s_dareg(array) as usize].push(regs.dregs[get_s_dreg(value)]);
        }

        Type::StringArr => {
            regs.saregs[get_s_sareg(array) as usize].push(regs.sregs[get_s_sreg(value)].clone());
        }

        Type::BoolArr => {
            regs.baregs[get_s_bareg(array) as usize].push(regs.bregs[get_s_breg(value)]);
        }
        
        Type::CharArr => {
            regs.caregs[get_s_careg(array) as usize].push(regs.cregs[get_s_creg(value)]);
        }

        _ => {
            panic!("not a valid array type");
        }
    }
}

fn get_array_at(regs: &mut Registers, aty: Type, array: u16, index: u16, target: u16) {
    match aty {
        Type::IntArr => {
            regs.iregs[get_s_ireg(target)] = regs.iaregs[get_s_iareg(array) as usize][regs.lregs[get_s_lreg(index)] as usize];
        }

        Type::LongArr => {
            regs.lregs[get_s_lreg(target)] = regs.laregs[get_s_lareg(array) as usize][regs.lregs[get_s_lreg(index)] as usize];
        }

        Type::FloatArr => {
            regs.fregs[get_s_freg(target)] = regs.faregs[get_s_fareg(array) as usize][regs.lregs[get_s_lreg(index)] as usize];
        }

        Type::DoubleArr => {
            regs.dregs[get_s_dreg(target)] = regs.daregs[get_s_dareg(array) as usize][regs.lregs[get_s_lreg(index)] as usize];
        }

        Type::StringArr => {
            regs.sregs[get_s_sreg(target)] = regs.saregs[get_s_sareg(array) as usize][regs.lregs[get_s_lreg(index)] as usize].clone();
        }

        Type::BoolArr => {
            regs.bregs[get_s_breg(target)] = regs.baregs[get_s_bareg(array) as usize][regs.lregs[get_s_lreg(index)] as usize];
        }
        
        Type::CharArr => {
            regs.cregs[get_s_creg(target)] = regs.caregs[get_s_careg(array) as usize][regs.lregs[get_s_lreg(index)] as usize];
        }

        _ => {
            panic!("not a valid array type");
        }
    }
}

fn set_array_at(regs: &mut Registers, aty: Type, array: u16, index: u16, value: u16) {
    match aty {
        Type::IntArr => {
            regs.iaregs[get_s_iareg(array) as usize][regs.lregs[get_s_lreg(index)] as usize] = get_val_int(regs, value);
        }

        Type::LongArr => {
            regs.laregs[get_s_lareg(array) as usize][regs.lregs[get_s_lreg(index)] as usize] = get_val_long(regs, value);
        }

        Type::FloatArr => {
            regs.faregs[get_s_fareg(array) as usize][regs.lregs[get_s_lreg(index)] as usize] = get_val_float(regs, value);
        }

        Type::DoubleArr => {
            regs.daregs[get_s_dareg(array) as usize][regs.lregs[get_s_lreg(index)] as usize] = get_val_double(regs, value);
        }

        Type::StringArr => {
            regs.saregs[get_s_sareg(array) as usize][regs.lregs[get_s_lreg(index)] as usize] = get_val_string(regs, value);
        }

        Type::BoolArr => {
            regs.baregs[get_s_bareg(array) as usize][regs.lregs[get_s_lreg(index)] as usize] = get_val_bool(regs, value);
        }
        
        Type::CharArr => {
            regs.caregs[get_s_careg(array) as usize][regs.lregs[get_s_lreg(index)] as usize] = get_val_char(regs, value);
        }

        _ => {
            panic!("not a valid array type");
        }
    }
}

fn array_type_match(array: Type, value: Type) -> bool {
    match array {
        Type::IntArr => {
            if value == Type::Int {
                return true;
            }
            return false;
        }

        Type::LongArr => {
            if value == Type::Long {
                return true;
            }
            return false;
        }

        Type::FloatArr => {
            if value == Type::Float {
                return true;
            }
            return false;
        }

        Type::DoubleArr => {
            if value == Type::Double {
                return true;
            }
            return false;
        }

        Type::StringArr => {
            if value == Type::String {
                return true;
            }
            return false;
        }

        Type::BoolArr => {
            if value == Type::Bool {
                return true;
            }
            return false;
        }
        
        Type::CharArr => {
            if value == Type::Char {
                return true;
            }
            return false;
        }

        _ => {
            panic!("not a valid array type");
        }
    }
}

fn get_reg_as_val(regs: &mut Registers, reg: u16) -> Val {
    let ty = get_type(reg);

    match ty {
        Type::Int => {
            return Val::Int(get_val_int(regs, reg));
        }

        Type::Long => {
            return Val::Long(get_val_long(regs, reg));
        }

        Type::Float => {
            return Val::Float(get_val_float(regs, reg));
        }

        Type::Double => {
            return Val::Double(get_val_double(regs, reg));
        }

        Type::String => {
            return Val::String(get_val_string(regs, reg));
        }

        Type::Bool => {
            return Val::Bool(get_val_bool(regs, reg));
        }

        Type::Char => {
            return Val::Char(get_val_char(regs, reg));
        }
        Type::IntArr => {
            return Val::IntArr(get_val_int_arr(regs, reg));
        }

        Type::LongArr => {
            return Val::LongArr(get_val_long_arr(regs, reg));
        }

        Type::FloatArr => {
            return Val::FloatArr(get_val_float_arr(regs, reg));
        }

        Type::DoubleArr => {
            return Val::DoubleArr(get_val_double_arr(regs, reg));
        }

        Type::StringArr => {
            return Val::StringArr(get_val_string_arr(regs, reg));
        }

        Type::BoolArr => {
            return Val::BoolArr(get_val_bool_arr(regs, reg));
        }

        Type::CharArr => {
            return Val::CharArr(get_val_char_arr(regs, reg));
        }
    }
}

fn not_bool(regs: &mut Registers, target: u16, lhs: u16) {
    let tty = get_type(target);
    let lty = get_type(lhs);

    if !(tty == Type::Bool && tty == lty) {
        panic!("all parts of boolean and have to be bool ({:?} = !{:?} is not possible)", tty, lty);
    }

    make_avail(regs, target);
    make_avail(regs, lhs);

    regs.bregs[get_s_breg(target)] = !regs.bregs[get_s_breg(lhs)];
}

fn or_bool(regs: &mut Registers, target: u16, lhs: u16, rhs: u16) {
    let tty = get_type(target);
    let lty = get_type(lhs);
    let rty = get_type(rhs);

    if !(tty == Type::Bool && tty == lty && lty == rty) {
        panic!("all parts of boolean or have to be bool ({:?} = {:?} || {:?} is not possible)", tty, lty, rty);
    }

    make_avail(regs, target);
    make_avail(regs, lhs);
    make_avail(regs, rhs);

    regs.bregs[get_s_breg(target)] = regs.bregs[get_s_breg(lhs)] || regs.bregs[get_s_breg(rhs)];
}

fn and_bool(regs: &mut Registers, target: u16, lhs: u16, rhs: u16) {
    let tty = get_type(target);
    let lty = get_type(lhs);
    let rty = get_type(rhs);

    if !(tty == Type::Bool && tty == lty && lty == rty) {
        panic!("all parts of boolean and have to be bool ({:?} = {:?} && {:?} is not possible)", tty, lty, rty);
    }

    make_avail(regs, target);
    make_avail(regs, lhs);
    make_avail(regs, rhs);

    regs.bregs[get_s_breg(target)] = regs.bregs[get_s_breg(lhs)] && regs.bregs[get_s_breg(rhs)];
}

fn check_less_than_equal(regs: &mut Registers, target: u16, lhs: u16, rhs: u16) {
    let tty = get_type(target);
    let lty = get_type(lhs);
    let rty = get_type(rhs);

    if tty != Type::Bool {
        panic!("target of comparison result has to be boolean");
    }

    if !(lty == rty) {
        panic!("can not compare different types ({:?} <= {:?} is not possible)", lty, rty);
    }

    make_avail(regs, target);
    make_avail(regs, lhs);
    make_avail(regs, rhs);

    match lty {
        Type::Int => {
            regs.bregs[get_s_breg(target)] = regs.iregs[get_s_ireg(lhs)] <= regs.iregs[get_s_ireg(rhs)];
        }

        Type::Long => {
            regs.bregs[get_s_breg(target)] = regs.lregs[get_s_lreg(lhs)] <= regs.lregs[get_s_lreg(rhs)];
        }

        Type::Float => {
            regs.bregs[get_s_breg(target)] = regs.fregs[get_s_freg(lhs)] <= regs.fregs[get_s_freg(rhs)];
        }

        Type::Double => {
            regs.bregs[get_s_breg(target)] = regs.dregs[get_s_dreg(lhs)] <= regs.dregs[get_s_dreg(rhs)];
        }

        Type::String => {
            panic!("can not compare 'string' types");
        }

        Type::Bool => {
            panic!("can not compare 'boolean' types");
        }

        Type::Char => {
            panic!("can not compare 'char' types");
        }
        Type::IntArr => {
            panic!("can not compare 'int array' types");
        }

        Type::LongArr => {
            panic!("can not compare 'long array' types");
        }

        Type::FloatArr => {
            panic!("can not compare 'float array' types");
        }

        Type::DoubleArr => {
            panic!("can not compare 'double array' tpyes");
        }

        Type::StringArr => {
            panic!("can not compare 'string array' types");
        }

        Type::BoolArr => {
            panic!("can not compare 'boolean array' types");
        }

        Type::CharArr => {
            panic!("can not compare 'char array' types");
        }
    }
}

fn check_greater_than_equal(regs: &mut Registers, target: u16, lhs: u16, rhs: u16) {
    let tty = get_type(target);
    let lty = get_type(lhs);
    let rty = get_type(rhs);

    if tty != Type::Bool {
        panic!("target of comparison result has to be boolean");
    }

    if !(lty == rty) {
        panic!("can not compare different types ({:?} >= {:?} is not possible)", lty, rty);
    }

    make_avail(regs, target);
    make_avail(regs, lhs);
    make_avail(regs, rhs);

    match lty {
        Type::Int => {
            regs.bregs[get_s_breg(target)] = regs.iregs[get_s_ireg(lhs)] >= regs.iregs[get_s_ireg(rhs)];
        }

        Type::Long => {
            regs.bregs[get_s_breg(target)] = regs.lregs[get_s_lreg(lhs)] >= regs.lregs[get_s_lreg(rhs)];
        }

        Type::Float => {
            regs.bregs[get_s_breg(target)] = regs.fregs[get_s_freg(lhs)] >= regs.fregs[get_s_freg(rhs)];
        }

        Type::Double => {
            regs.bregs[get_s_breg(target)] = regs.dregs[get_s_dreg(lhs)] >= regs.dregs[get_s_dreg(rhs)];
        }

        Type::String => {
            panic!("can not compare 'string' types");
        }

        Type::Bool => {
            panic!("can not compare 'boolean' types");
        }

        Type::Char => {
            panic!("can not compare 'char' types");
        }
        Type::IntArr => {
            panic!("can not compare 'int array' types");
        }

        Type::LongArr => {
            panic!("can not compare 'long array' types");
        }

        Type::FloatArr => {
            panic!("can not compare 'float array' types");
        }

        Type::DoubleArr => {
            panic!("can not compare 'double array' tpyes");
        }

        Type::StringArr => {
            panic!("can not compare 'string array' types");
        }

        Type::BoolArr => {
            panic!("can not compare 'boolean array' types");
        }

        Type::CharArr => {
            panic!("can not compare 'char array' types");
        }
    }
}

fn check_less_than(regs: &mut Registers, target: u16, lhs: u16, rhs: u16) {
    let tty = get_type(target);
    let lty = get_type(lhs);
    let rty = get_type(rhs);

    if tty != Type::Bool {
        panic!("target of comparison result has to be boolean");
    }

    if !(lty == rty) {
        panic!("can not compare different types ({:?} < {:?} is not possible)", lty, rty);
    }

    make_avail(regs, target);
    make_avail(regs, lhs);
    make_avail(regs, rhs);

    match lty {
        Type::Int => {
            regs.bregs[get_s_breg(target)] = regs.iregs[get_s_ireg(lhs)] < regs.iregs[get_s_ireg(rhs)];
        }

        Type::Long => {
            regs.bregs[get_s_breg(target)] = regs.lregs[get_s_lreg(lhs)] < regs.lregs[get_s_lreg(rhs)];
        }

        Type::Float => {
            regs.bregs[get_s_breg(target)] = regs.fregs[get_s_freg(lhs)] < regs.fregs[get_s_freg(rhs)];
        }

        Type::Double => {
            regs.bregs[get_s_breg(target)] = regs.dregs[get_s_dreg(lhs)] < regs.dregs[get_s_dreg(rhs)];
        }

        Type::String => {
            panic!("can not compare 'string' types");
        }

        Type::Bool => {
            panic!("can not compare 'boolean' types");
        }

        Type::Char => {
            panic!("can not compare 'char' types");
        }
        Type::IntArr => {
            panic!("can not compare 'int array' types");
        }

        Type::LongArr => {
            panic!("can not compare 'long array' types");
        }

        Type::FloatArr => {
            panic!("can not compare 'float array' types");
        }

        Type::DoubleArr => {
            panic!("can not compare 'double array' tpyes");
        }

        Type::StringArr => {
            panic!("can not compare 'string array' types");
        }

        Type::BoolArr => {
            panic!("can not compare 'boolean array' types");
        }

        Type::CharArr => {
            panic!("can not compare 'char array' types");
        }
    }
}

fn check_greater_than(regs: &mut Registers, target: u16, lhs: u16, rhs: u16) {
    let tty = get_type(target);
    let lty = get_type(lhs);
    let rty = get_type(rhs);

    if tty != Type::Bool {
        panic!("target of comparison result has to be boolean");
    }

    if !(lty == rty) {
        panic!("can not compare different types ({:?} > {:?} is not possible)", lty, rty);
    }

    make_avail(regs, target);
    make_avail(regs, lhs);
    make_avail(regs, rhs);

    match lty {
        Type::Int => {
            regs.bregs[get_s_breg(target)] = regs.iregs[get_s_ireg(lhs)] > regs.iregs[get_s_ireg(rhs)];
        }

        Type::Long => {
            regs.bregs[get_s_breg(target)] = regs.lregs[get_s_lreg(lhs)] > regs.lregs[get_s_lreg(rhs)];
        }

        Type::Float => {
            regs.bregs[get_s_breg(target)] = regs.fregs[get_s_freg(lhs)] > regs.fregs[get_s_freg(rhs)];
        }

        Type::Double => {
            regs.bregs[get_s_breg(target)] = regs.dregs[get_s_dreg(lhs)] > regs.dregs[get_s_dreg(rhs)];
        }

        Type::String => {
            panic!("can not compare 'string' types");
        }

        Type::Bool => {
            panic!("can not compare 'boolean' types");
        }

        Type::Char => {
            panic!("can not compare 'char' types");
        }
        Type::IntArr => {
            panic!("can not compare 'int array' types");
        }

        Type::LongArr => {
            panic!("can not compare 'long array' types");
        }

        Type::FloatArr => {
            panic!("can not compare 'float array' types");
        }

        Type::DoubleArr => {
            panic!("can not compare 'double array' tpyes");
        }

        Type::StringArr => {
            panic!("can not compare 'string array' types");
        }

        Type::BoolArr => {
            panic!("can not compare 'boolean array' types");
        }

        Type::CharArr => {
            panic!("can not compare 'char array' types");
        }
    }
}

fn make_avail(regs: &mut Registers, r: u16) {
    let ty = get_type(r);

    match ty {
        Type::Int => {
            let idx = get_s_ireg(r);
            let slen = regs.iregs.len();
            if idx == slen {
                regs.iregs.push(0);
            }
            if idx > slen {
                panic!("the register number {} is too big for iregs len {} (not in increasing order)", idx, slen);
            }
        }

        Type::Long => {
            let idx = get_s_lreg(r);
            let slen = regs.lregs.len();
            if idx == slen {
                regs.lregs.push(0);
            }
            if idx > slen {
                panic!("the register number {} is too big for lregs len {} (not in increasing order)", idx, slen);
            }
        }

        Type::Float => {
            let idx = get_s_freg(r);
            let slen = regs.fregs.len();
            if idx == slen {
                regs.fregs.push(0.0);
            }
            if idx > slen {
                panic!("the register number {} is too big for fregs len {} (not in increasing order)", idx, slen);
            }
        }

        Type::Double => {
            let idx = get_s_dreg(r);
            let slen = regs.dregs.len();
            if idx == slen {
                regs.dregs.push(0.0);
            }
            if idx > slen {
                panic!("the register number {} is too big for dregs len {} (not in increasing order)", idx, slen);
            }
        }

        Type::String => {
            let idx = get_s_sreg(r);
            let slen = regs.sregs.len();
            if idx == slen {
                regs.sregs.push("".to_owned());
            }
            if idx > slen {
                panic!("the register number {} is too big for sregs len {} (not in increasing order)", idx, slen);
            }
        }

        Type::Bool => {
            let idx = get_s_breg(r);
            let slen = regs.bregs.len();
            if idx == slen {
                regs.bregs.push(false);
            }
            if idx > slen {
                panic!("the register number {} is too big for bregs len {} (not in increasing order)", idx, slen);
            }
        }

        Type::Char => {
            let idx = get_s_creg(r);
            let slen = regs.cregs.len();
            if idx == slen {
                regs.cregs.push(0 as char);
            }
            if idx > slen {
                panic!("the register number {} is too big for cregs len {} (not in increasing order)", idx, slen);
            }
        }

        _ => {
            todo!("implement allocation for array registers");
        }
    }
}

fn check_equal(regs: &mut Registers, target: u16, lhs: u16, rhs: u16) {
    let tty = get_type(target);
    let lty = get_type(lhs);
    let rty = get_type(rhs);

    if tty != Type::Bool {
        panic!("target in equality check has to be bool");
    }

    if lty != rty {
        panic!("types must match in equality check ({:?} == {:?} is not possible)", rty, lty);
    }

    make_avail(regs, target);
    make_avail(regs, lhs);
    make_avail(regs, rhs);

    match lty {
        Type::Int => {
            regs.bregs[get_s_breg(target)] = regs.iregs[get_s_ireg(lhs)] == regs.iregs[get_s_ireg(rhs)];
        }

        Type::Long => {
            regs.bregs[get_s_breg(target)] = regs.lregs[get_s_lreg(lhs)] == regs.lregs[get_s_lreg(rhs)];
        }

        Type::Float => {
            regs.bregs[get_s_breg(target)] = regs.fregs[get_s_freg(lhs)] == regs.fregs[get_s_freg(rhs)];
        }

        Type::Double => {
            regs.bregs[get_s_breg(target)] = regs.dregs[get_s_dreg(lhs)] == regs.dregs[get_s_dreg(rhs)];
        }

        Type::String => {
            regs.bregs[get_s_breg(target)] = regs.sregs[get_s_sreg(lhs)] == regs.sregs[get_s_sreg(rhs)];
        }

        Type::Bool => {
            regs.bregs[get_s_breg(target)] = regs.bregs[get_s_breg(lhs)] == regs.bregs[get_s_breg(rhs)];
        }

        Type::Char => {
            regs.bregs[get_s_breg(target)] = regs.cregs[get_s_creg(lhs)] == regs.cregs[get_s_creg(rhs)];
        }

        Type::IntArr => {
            regs.bregs[get_s_breg(target)] = regs.iaregs[get_s_iareg(lhs)] == regs.iaregs[get_s_iareg(rhs)];
        }

        Type::LongArr => {
            regs.bregs[get_s_breg(target)] = regs.laregs[get_s_lareg(lhs)] == regs.laregs[get_s_lareg(rhs)];
        }

        Type::FloatArr => {
            regs.bregs[get_s_breg(target)] = regs.faregs[get_s_fareg(lhs)] == regs.faregs[get_s_fareg(rhs)];
        }

        Type::DoubleArr => {
            regs.bregs[get_s_breg(target)] = regs.daregs[get_s_dareg(lhs)] == regs.daregs[get_s_dareg(rhs)];
        }

        Type::StringArr => {
            regs.bregs[get_s_breg(target)] = regs.saregs[get_s_sareg(lhs)] == regs.saregs[get_s_sareg(rhs)];
        }

        Type::BoolArr => {
            regs.bregs[get_s_breg(target)] = regs.baregs[get_s_bareg(lhs)] == regs.baregs[get_s_bareg(rhs)];
        }

        Type::CharArr => {
            regs.bregs[get_s_breg(target)] = regs.dregs[get_s_careg(lhs)] == regs.dregs[get_s_careg(rhs)];
        }
    }
}

fn devide_checked(regs: &mut Registers, target: u16, lhs: u16, rhs: u16) {
    let tty = get_type(target);
    let lty = get_type(lhs);
    let rty = get_type(rhs);

    if !(tty == lty && lty == rty) {
        panic!("cannot devide types with {:?} = {:?} / {:?}", tty, lty, rty);
    }

    make_avail(regs, target);
    make_avail(regs, lhs);
    make_avail(regs, rhs);

    match tty {
        Type::Int => {
            regs.iregs[get_s_ireg(target)] = regs.iregs[get_s_ireg(lhs)] / regs.iregs[get_s_ireg(rhs)];
        }

        Type::Long => {
            regs.lregs[get_s_lreg(target)] = regs.lregs[get_s_lreg(lhs)] / regs.lregs[get_s_lreg(rhs)];
        }

        Type::Float => {
            regs.fregs[get_s_freg(target)] = regs.fregs[get_s_freg(lhs)] / regs.fregs[get_s_freg(rhs)];
        }

        Type::Double => {
            regs.dregs[get_s_dreg(target)] = regs.dregs[get_s_dreg(lhs)] / regs.dregs[get_s_dreg(rhs)];
        }

        Type::String => {
            panic!("can not devide 'string' types");
        }

        Type::Bool => {
            panic!("can not devide 'boolean' types");
        }

        Type::Char => {
            panic!("can not devide 'char' types");
        }
        Type::IntArr => {
            panic!("can not devide 'int array' types");
        }

        Type::LongArr => {
            panic!("can not devide 'long array' types");
        }

        Type::FloatArr => {
            panic!("can not devide 'float array' types");
        }

        Type::DoubleArr => {
            panic!("can not devide 'double array' tpyes");
        }

        Type::StringArr => {
            panic!("can not devide 'string array' types");
        }

        Type::BoolArr => {
            panic!("can not devide 'boolean array' types");
        }

        Type::CharArr => {
            panic!("can not devide 'char array' types");
        }
    }
}

fn multiply_checked(regs: &mut Registers, target: u16, lhs: u16, rhs: u16) {
    let tty = get_type(target);
    let lty = get_type(lhs);
    let rty = get_type(rhs);

    if !(tty == lty && lty == rty) {
        panic!("cannot multiply types with {:?} = {:?} * {:?}", tty, lty, rty);
    }

    make_avail(regs, target);
    make_avail(regs, lhs);
    make_avail(regs, rhs);

    match tty {
        Type::Int => {
            regs.iregs[get_s_ireg(target)] = regs.iregs[get_s_ireg(lhs)] * regs.iregs[get_s_ireg(rhs)];
        }

        Type::Long => {
            regs.lregs[get_s_lreg(target)] = regs.lregs[get_s_lreg(lhs)] * regs.lregs[get_s_lreg(rhs)];
        }

        Type::Float => {
            regs.fregs[get_s_freg(target)] = regs.fregs[get_s_freg(lhs)] * regs.fregs[get_s_freg(rhs)];
        }

        Type::Double => {
            regs.dregs[get_s_dreg(target)] = regs.dregs[get_s_dreg(lhs)] * regs.dregs[get_s_dreg(rhs)];
        }

        Type::String => {
            panic!("can not multiply 'string' types");
        }

        Type::Bool => {
            panic!("can not multiply 'boolean' types");
        }

        Type::Char => {
            panic!("can not multiply 'char' types");
        }
        Type::IntArr => {
            panic!("can not multiply 'int array' types");
        }

        Type::LongArr => {
            panic!("can not multiply 'long array' types");
        }

        Type::FloatArr => {
            panic!("can not multiply 'float array' types");
        }

        Type::DoubleArr => {
            panic!("can not multiply 'double array' tpyes");
        }

        Type::StringArr => {
            panic!("can not multiply 'string array' types");
        }

        Type::BoolArr => {
            panic!("can not multiply 'boolean array' types");
        }

        Type::CharArr => {
            panic!("can not multiply 'char array' types");
        }
    }
}

fn subtract_checked(regs: &mut Registers, target: u16, lhs: u16, rhs: u16) {
    let tty = get_type(target);
    let lty = get_type(lhs);
    let rty = get_type(rhs);

    if !(tty == lty && lty == rty) {
        panic!("cannot subtract types with {:?} = {:?} - {:?}", tty, lty, rty);
    }

    make_avail(regs, target);
    make_avail(regs, lhs);
    make_avail(regs, rhs);

    match tty {
        Type::Int => {
            regs.iregs[get_s_ireg(target)] = regs.iregs[get_s_ireg(lhs)] - regs.iregs[get_s_ireg(rhs)];
        }

        Type::Long => {
            regs.lregs[get_s_lreg(target)] = regs.lregs[get_s_lreg(lhs)] - regs.lregs[get_s_lreg(rhs)];
        }

        Type::Float => {
            regs.fregs[get_s_freg(target)] = regs.fregs[get_s_freg(lhs)] - regs.fregs[get_s_freg(rhs)];
        }

        Type::Double => {
            regs.dregs[get_s_dreg(target)] = regs.dregs[get_s_dreg(lhs)] - regs.dregs[get_s_dreg(rhs)];
        }

        Type::String => {
            panic!("cannot subtract 'string' types");
        }

        Type::Bool => {
            panic!("can not subtract 'boolean' types");
        }

        Type::Char => {
            panic!("can not subtract 'char' types");
        }
        Type::IntArr => {
            panic!("can not subtract 'int array' types");
        }

        Type::LongArr => {
            panic!("can not subtract 'long array' types");
        }

        Type::FloatArr => {
            panic!("can not subtract 'float array' types");
        }

        Type::DoubleArr => {
            panic!("can not subtract 'double array' tpyes");
        }

        Type::StringArr => {
            panic!("can not subtract 'string array' types");
        }

        Type::BoolArr => {
            panic!("can not subtract 'boolean array' types");
        }

        Type::CharArr => {
            panic!("can not subtract 'char array' types");
        }
    }
}

fn add_checked(regs: &mut Registers, target: u16, lhs: u16, rhs: u16) {
    let tty = get_type(target);
    let lty = get_type(lhs);
    let rty = get_type(rhs);

    if !(tty == lty && lty == rty) {
        panic!("cannot add types with {:?} = {:?} + {:?}", tty, lty, rty);
    }

    make_avail(regs, target);
    make_avail(regs, lhs);
    make_avail(regs, rhs);

    match tty {
        Type::Int => {
            regs.iregs[get_s_ireg(target)] = regs.iregs[get_s_ireg(lhs)] + regs.iregs[get_s_ireg(rhs)];
        }

        Type::Long => {
            regs.lregs[get_s_lreg(target)] = regs.lregs[get_s_lreg(lhs)] + regs.lregs[get_s_lreg(rhs)];
        }

        Type::Float => {
            regs.fregs[get_s_freg(target)] = regs.fregs[get_s_freg(lhs)] + regs.fregs[get_s_freg(rhs)];
        }

        Type::Double => {
            regs.dregs[get_s_dreg(target)] = regs.dregs[get_s_dreg(lhs)] + regs.dregs[get_s_dreg(rhs)];
        }

        Type::String => {
            regs.sregs[get_s_sreg(target)] = regs.sregs[get_s_sreg(lhs)].clone() + regs.sregs[get_s_sreg(rhs)].as_str();
        }

        Type::Bool => {
            panic!("can not add 'boolean' types");
        }

        Type::Char => {
            panic!("can not add 'char' types");
        }
        Type::IntArr => {
            panic!("can not add 'int array' types");
        }

        Type::LongArr => {
            panic!("can not add 'long array' types");
        }

        Type::FloatArr => {
            panic!("can not add 'float array' types");
        }

        Type::DoubleArr => {
            panic!("can not add 'double array' tpyes");
        }

        Type::StringArr => {
            panic!("can not add 'string array' types");
        }

        Type::BoolArr => {
            panic!("can not add 'boolean array' types");
        }

        Type::CharArr => {
            panic!("can not add 'char array' types");
        }
    }
}

/// gets the next 16 bits of the function and returns them as a u16
fn get_following_16bit(i: &mut usize, func: &Vec<u8>) -> u16 {
    let f = (func[*i+1] as u16)<< 8;
    let s = func[*i+2] as u16;
    *i+=2;
    return f | s;
}

/// gets the next 16 bits of the function and returns them as a u16
fn get_following_i16(i: &mut usize, func: &Vec<u8>) -> i16 {
    let f = (func[*i+1] as i16)<< 8;
    let s = func[*i+2] as i16;
    *i+=2;
    return f | s;
}

/// sets a register to a Val
fn set_reg_to_val(regs: &mut Registers, r: u16, val: Val) {
    let ty = get_type(r);

    match ty {
        Type::Int => {
            if let Val::Int(v) = val {
                let idx = get_s_ireg(r);
                if idx < regs.iregs.len() {
                    regs.iregs[idx] = v;
                    return;
                }
                if idx == regs.iregs.len() {
                    regs.iregs.push(v);
                }
                else {
                    panic!("the index {} is to large for iregs with length {}", idx, regs.iregs.len());
                }
            }
        }

        Type::Long => {
            if let Val::Long(v) = val {
                let idx = get_s_lreg(r);
                if idx < regs.lregs.len() {
                    regs.lregs[idx] = v;
                    return;
                }
                if idx == regs.lregs.len() {
                    regs.lregs.push(v);
                }
                else {
                    panic!("the index {} is to large for lregs with length {}", idx, regs.lregs.len());
                }
            }
        }

        Type::Float => {
            if let Val::Float(v) = val {
                let idx = get_s_freg(r);
                if idx < regs.fregs.len() {
                    regs.fregs[idx] = v;
                    return;
                }
                if idx == regs.fregs.len() {
                    regs.fregs.push(v);
                }
                else {
                    panic!("the index {} is to large for fregs with length {}", idx, regs.fregs.len());
                }
            }
        }

        Type::Double => {
            if let Val::Double(v) = val {
                let idx = get_s_freg(r);
                if idx < regs.dregs.len() {
                    regs.dregs[idx] = v;
                    return;
                }
                if idx == regs.dregs.len() {
                    regs.dregs.push(v);
                }
                else {
                    panic!("the index {} is to large for dregs with length {}", idx, regs.dregs.len());
                }
            }
        }

        Type::String => {
            if let Val::String(v) = val {
                let idx = get_s_sreg(r);
                if idx < regs.sregs.len() {
                    regs.sregs[idx] = v;
                    return;
                }
                if idx == regs.sregs.len() {
                    regs.sregs.push(v);
                }
                else {
                    panic!("the index {} is to large for sregs with length {}", idx, regs.sregs.len());
                }
            }
        }

        Type::Bool => {
            if let Val::Bool(v) = val {
                let idx = get_s_breg(r);
                if idx < regs.bregs.len() {
                    regs.bregs[idx] = v;
                    return;
                }
                if idx == regs.bregs.len() {
                    regs.bregs.push(v);
                }
                else {
                    panic!("the index {} is to large for bregs with length {}", idx, regs.bregs.len());
                }
            }
        }

        Type::Char => {
            if let Val::Char(v) = val {
                let idx = get_s_creg(r);
                if idx < regs.cregs.len() {
                    regs.cregs[idx] = v;
                    return;
                }
                if idx == regs.cregs.len() {
                    regs.cregs.push(v);
                }
                else {
                    panic!("the index {} is to large for cregs with length {}", idx, regs.cregs.len());
                }
            }
        }

        // TODO: add checking to array register types
        Type::IntArr => {
            if let Val::IntArr(v) = val {
                regs.iaregs[get_s_iareg(r)] = v;
            }
        }

        Type::LongArr => {
            if let Val::LongArr(v) = val {
                regs.laregs[get_s_lareg(r)] = v;
            }
        }

        Type::FloatArr => {
            if let Val::FloatArr(v) = val {
                regs.faregs[get_s_fareg(r)] = v;
            }
        }

        Type::DoubleArr => {
            if let Val::DoubleArr(v) = val {
                regs.daregs[get_s_dareg(r)] = v;
            }
        }

        Type::StringArr => {
            if let Val::StringArr(v) = val {
                regs.saregs[get_s_sareg(r)] = v;
            }
        }

        Type::BoolArr => {
            if let Val::BoolArr(v) = val {
                regs.baregs[get_s_bareg(r)] = v;
            }                    }

        Type::CharArr => {
            if let Val::CharArr(v) = val {
                regs.caregs[get_s_careg(r)] = v;
            }
        }
    }
}

///returns the type of a register
pub fn get_type(r: u16) -> Type {
    if r < 4681 {
        return Type::Int;
    }
    if r < 9362 {
        return Type::Long;
    }
    if r < 14043 {
        return Type::Float;
    }
    if r < 18724 {
        return Type::Double;
    }
    if r < 23405 {
        return Type::String;
    }
    if r < 28086 {
        return Type::Bool;
    }
    if r < 32767 {
        return Type::Char;
    }
    if r < 37448 {
        return Type::IntArr;
    }
    if r < 42129 {
        return Type::LongArr;
    }
    if r < 46810 {
        return Type::FloatArr;
    }
    if r < 51491 {
        return Type::DoubleArr;
    }
    if r < 56172 {
        return Type::StringArr;
    }
    if r < 60853 {
        return Type::BoolArr;
    }
    if r < 65534 {
        return Type::CharArr;
    }

    panic!("invalid register");
}
/// prints a register
fn print_reg(regs: &Registers, r: u16) {
    let ty = get_type(r);

    match ty {
        Type::Int => {
            println!("{}", get_val_int(regs, r));
        }

        Type::Long => {
            println!("{}", get_val_long(regs, r));
        }

        Type::Float => {
            println!("{}", get_val_float(regs, r));
        }

        Type::Double => {
            println!("{}", get_val_double(regs, r));
        }

        Type::String => {
            println!("{}", get_val_string(regs, r));
        }

        Type::Bool => {
            println!("{}", get_val_bool(regs, r));
        }

        Type::Char => {
            println!("{}", get_val_char(regs, r));
        }
        Type::IntArr => {
            println!("{:?}", get_val_int_arr(regs, r));
        }

        Type::LongArr => {
            println!("{:?}", get_val_long_arr(regs, r));
        }

        Type::FloatArr => {
            println!("{:?}", get_val_float_arr(regs, r));
        }

        Type::DoubleArr => {
            println!("{:?}", get_val_double_arr(regs, r));
        }

        Type::StringArr => {
            println!("{:?}", get_val_string_arr(regs, r));
        }

        Type::BoolArr => {
            println!("{:?}", get_val_bool_arr(regs, r));
        }

        Type::CharArr => {
            println!("{:?}", get_val_char_arr(regs, r));
        }
    }
}

/// returns the value of a int register
fn get_val_int(regs: &Registers, r: u16) -> i32 {
    return regs.iregs[get_s_ireg(r)];
}
/// returns the value of a long register
fn get_val_long(regs: &Registers, r: u16) -> i64 {
    return regs.lregs[get_s_lreg(r)];
}
/// returns the value of a float register
fn get_val_float(regs: &Registers, r: u16) -> f32 {
    return regs.fregs[get_s_freg(r)];
}
/// returns the value of a double register
fn get_val_double(regs: &Registers, r: u16) -> f64 {
    return regs.dregs[get_s_dreg(r)];
}
/// returns the value of a string register
fn get_val_string(regs: &Registers, r: u16) -> String {
    return regs.sregs[get_s_sreg(r)].clone();
}
/// returns the value of a bool register
fn get_val_bool(regs: &Registers, r: u16) -> bool {
    return regs.bregs[get_s_breg(r)];
}
/// returns the value of a char register
fn get_val_char(regs: &Registers, r: u16) -> char {
    return regs.cregs[get_s_creg(r)];
}
/// returns the value of a int array register
fn get_val_int_arr(regs: &Registers, r: u16) -> Vec<i32> {
    return regs.iaregs[get_s_iareg(r)].clone();
}
/// returns the value of a logn array register
fn get_val_long_arr(regs: &Registers, r: u16) -> Vec<i64> {
    return regs.laregs[get_s_lareg(r)].clone();
}
/// returns the value of a float array register
fn get_val_float_arr(regs: &Registers, r: u16) -> Vec<f32> {
    return regs.faregs[get_s_fareg(r)].clone();
}
/// returns the value of a double array register
fn get_val_double_arr(regs: &Registers, r: u16) -> Vec<f64> {
    return regs.daregs[get_s_dareg(r)].clone();
}
/// returns the value of a string array register
fn get_val_string_arr(regs: &Registers, r: u16) -> Vec<String> {
    return regs.saregs[get_s_sareg(r)].clone();
}
/// returns the value of a boolean array register
fn get_val_bool_arr(regs: &Registers, r: u16) -> Vec<bool> {
    return regs.baregs[get_s_bareg(r)].clone();
}
/// returns the value of a char array register
fn get_val_char_arr(regs: &Registers, r: u16) -> Vec<char> {
    return regs.caregs[get_s_careg(r)].clone();
}

pub mod type_s {
    /// returns the local version of the int register
    pub fn get_s_ireg(r: u16) -> usize {
        return r as usize;
    }

    /// returns the local version of the long register
    pub fn get_s_lreg(r: u16) -> usize {
        return r as usize - 4681;
    }

    /// returns the local version of the float register
    pub fn get_s_freg(r: u16) -> usize {
        return r as usize - 4681 * 2;
    }

    /// returns the local version of the double register
    pub fn get_s_dreg(r: u16) -> usize {
        return r as usize - 4681 * 3;
    }

    /// returns the local version of the string register
    pub fn get_s_sreg(r: u16) -> usize {
        return r as usize - 4681 * 4;
    }

    /// returns the local version of the bool register
    pub fn get_s_breg(r: u16) -> usize {
        return r as usize - 4681 * 5;
    }

    /// returns the local version of the char register
    pub fn get_s_creg(r: u16) -> usize {
        return r as usize - 4681 * 6;
    }

    /// returns the local version of the int array register
    pub fn get_s_iareg(r: u16) -> usize {
        return r as usize - 4681 * 7;
    }

    /// returns the local version of the long array register
    pub fn get_s_lareg(r: u16) -> usize {
        return r as usize - 4681 * 8;
    }

    /// returns the local version of the float array register
    pub fn get_s_fareg(r: u16) -> usize {
        return r as usize - 4681 * 9;
    }

    /// returns the local version of the double array register
    pub fn get_s_dareg(r: u16) -> usize {
        return r as usize - 4681 * 10;
    }

    /// returns the local version of the string array register
    pub fn get_s_sareg(r: u16) -> usize {
        return r as usize - 4681 * 11;
    }

    /// returns the local version of the bool array register
    pub fn get_s_bareg(r: u16) -> usize {
        return r as usize - 4681 * 12;
    }

    /// returns the local version of the char array register
    pub fn get_s_careg(r: u16) -> usize {
        return r as usize - 4681 * 13;
    }
}