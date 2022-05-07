use std::fmt::Debug;
use std::collections::HashMap;
use std::fs;
use std::time::{Duration, Instant};

use serde::{Serialize, Deserialize};
use serde_json::Result;

use crate::bcasm::assemble_bc;
use crate::ast::parse_to_ast;
use crate::ast;
use crate::TarParser;
use crate::Rule;

use pest::Parser;
use pest::iterators::Pairs;

const MAX_RECURSION_DEPTH: u8 = 200;
#[non_exhaustive]
pub struct BCInst;

impl BCInst {
    /// empty operation
    pub const NOP: u8               = 0x00; // 0    
    /// stack: [..., value] prints the top val
    pub const PRINT: u8             = 0x01; // 0    
    /// loads a local const
    pub const LOAD_CONST: u8        = 0x02; // 1    
    /// stack: [..., value1, value2] adds the top to numbers and pushes the result
    pub const ADD: u8               = 0x03; // 0    
    /// stack: [..., value1, value2] subtracts the top two numbers and pushes the result
    pub const SUB: u8               = 0x04; // 0    
    /// stack: [..., value1, value2] multiplies the top two numbers
    pub const MUL: u8               = 0x05; // 0    
    /// stack: [..., value1, value2] multiplies the top two numbers
    pub const DIV: u8               = 0x06; // 0    
    /// stack: [..., value1, value2] checks if the last two values are qual
    pub const EQUAL: u8             = 0x07; // 0    
    // stack: [..., greater, lesser] checks if the second to top value is greater than the top value and pushes the result
    pub const GREATER_THAN: u8      = 0x08; // 0    
    /// stack: [..., lesser, greater] checks if the second to top value is less than the top value and pushes the result
    pub const LESS_THAN: u8         = 0x09; // 0    
    // stack: [..., greater, lesser] checks if the second to top value is greater than the top value and pushes the result
    pub const GREATER_THAN_EQUAL: u8= 0x0a; // 0    
    /// stack: [..., lesser, greater] checks if the second to top value is less than the top value and pushes the result
    pub const LESS_THAN_EQUAL: u8   = 0x0b; // 0    
    /// stack: [..., value1, value2] the last two values have to be bools; ors the last two vals and pushes the result
    pub const OR: u8                = 0x0c; // 0    
    /// stack: [..., value1, value2] the last two values have to be bools; ands the last two vals and pushes the result
    pub const AND: u8               = 0x0d; // 0    
    /// stack: [..., value1] the last val has to be a bool; inverts the last val on the stack
    pub const NOT: u8               = 0x0e; // 0    
    /// stack: [..., value1] stores the top val in the local env
    pub const STORE_LOCAL_VAL: u8   = 0x0f; // 1    
    /// pushes the specified local env val
    pub const LOAD_LOCAL_VAL: u8    = 0x10; // 1    
    /// stack: [..., arg1, arg2, ..., func name] has to be called after the function args have been pushed onto the stack; calls function named ontop of the stack
    pub const CALL_FUNC: u8         = 0x11; // 0    
    /// jumps to the value specified; all jumps: 1st word is 0 -> jump forward; first word is 1 -> jump backward; second word: how many bytes follow (these encode the actual distance can be 1 2 4 or 8)
    pub const JUMP: u8              = 0x12; // 2-?  
    /// jumps by the amount specified if the top value on the stack is true
    pub const JUMP_IF_TRUE: u8      = 0x13; // 2-?  
    /// jumps by the amount specified if the top value on the stack is false
    pub const JUMP_IF_FALSE: u8     = 0x14; // 2-?  
    /// stack: [..., array, index, new value] inserts the new value at the index into the array (out of bounds exception)
    pub const INSERT_ARR_AT: u8     = 0x15; // 0
    /// stack: [..., array, index] gets the new value at the index into the array (out of bounds exception)
    pub const GET_ARR_AT: u8        = 0x16; // 0
    /// stack: [..., array, value] pushes the value onto the array
    pub const PUSH_TO_ARR: u8       = 0x17; // 0    
    /// stack: [..., array] gets the value from an array
    pub const POP_FROM_ARR: u8      = 0x18; // 0    
    /// stack: [..., array] loads the length of a value onto the stack
    pub const LOAD_ARR_LEN: u8      = 0x19; // 0
    /// stack: [..., valn, ... val0, type, n] creates a new array with len n and a specified type and inserts preceding values
    pub const MAKE_ARR: u8          = 0x1a; // 0
    /// returns from a function 
    pub const RET: u8               = 0x1b; // 0
    /// starts a profiling timer
    pub const START_PROF: u8        = 0x1d; // 0
    /// stops a currently running profiling timer
    pub const STOP_PROF: u8         = 0x1e; // 0
}

#[derive(Clone, Debug, Serialize, Deserialize, std::cmp::PartialEq)]
pub enum Val {
    Int(i32),
    Long(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Array{
        ty: Type,
        arr: Vec<Val>
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, std::cmp::PartialEq)]
pub enum Type {
    Int,
    Long,
    Float,
    String,
    Bool,
    Array,
}

fn print_val(val: Val) {
    match val {
        Val::Int(v) => {
            println!("{}", v);
        }
        Val::Long(v) => {
            println!("{}", v);
        }
        Val::Float(v) => {
            println!("{}", v);
        }
        Val::String(v) => {
            println!("{}", v);
        }
        Val::Bool(v) => {
            println!("{}", v);
        }
        Val::Array{ty, arr} => {
            print!("[");
            for i in arr {
                print_val_nl(i);
                print!(", ");
            }
            print!("]");
            println!();
        }
    }
}

fn print_val_nl(val: Val) {
    match val {
        Val::Int(v) => {
            print!("{}", v);
        }
        Val::Long(v) => {
            print!("{}", v);
        }
        Val::Float(v) => {
            print!("{}", v);
        }
        Val::String(v) => {
            print!("{}", v);
        }
        Val::Bool(v) => {
            print!("{}", v);
        }
        Val::Array{ty, arr} => {
            print!("[");
            for i in arr {
                print_val_nl(i);
                print!(", ");
            }
            print!("]");
        }
    }
}

#[allow(unused)]
pub fn run_file_checked(path: &str) -> Result<()> {
    if !(&path[path.len()-3..path.len()] == "tar") {panic!()}

    let program = fs::read_to_string(path).unwrap();
    let mut path = (&path[0..path.len() - 3] as &str).to_owned();
    path.push_str("lock");
    let lock = fs::read_to_string(path.clone());

    let hash1 = format!("{:x}", md5::compute(program.clone())).to_owned();

    if lock.is_ok() {
        let (funcs, mut stack, entry, hash2): (HashMap<String, (Vec<u8>, Vec<Val>)>, Vec<Val>, Option<String>, String) = serde_json::from_str(lock.unwrap().as_str())?;
        if hash1 == hash2 {
            if entry.is_some() {
                run_func(entry.unwrap(), &funcs, &mut stack, 0);
            }
        }
        else {    
            let pairs: Pairs<Rule> = TarParser::parse(Rule::Program, program.as_str()).unwrap();
            let (defs, funcs): (Vec<ast::AstNode>, HashMap<String, ast::AstNode>) = parse_to_ast(pairs);
            let (funcs, mut stack, entry) = assemble_bc(defs, funcs);
            store_bc(path, funcs.clone(), stack.clone(), entry.clone(), hash1);
    
            if entry.is_some() {
                run_func(entry.unwrap(), &funcs, &mut stack, 0);
            }
        }
    }
    else {    
        let pairs: Pairs<Rule> = TarParser::parse(Rule::Program, program.as_str()).unwrap();
        let (defs, funcs): (Vec<ast::AstNode>, HashMap<String, ast::AstNode>) = parse_to_ast(pairs);
        let (funcs, mut stack, entry) = assemble_bc(defs, funcs);
        store_bc(path, funcs.clone(), stack.clone(), entry.clone(), hash1);

        if entry.is_some() {
            run_func(entry.unwrap(), &funcs, &mut stack, 0);
        }
    }

    return Ok(());
}

#[allow(unused)]
pub fn store_bc(path: String, funcs: HashMap<String, (Vec<u8>, Vec<Val>)>, stack: Vec<Val>, entry: Option<String>, hash: String) {
    let j = serde_json::to_string(&(funcs.clone(), stack.clone(), entry, hash)).unwrap();

    fs::write(path, j).unwrap();
}

#[allow(unused)]
pub fn run_from_string(string: String) -> Result<()> {
    let (funcs, mut stack, entry, hash): (HashMap<String, (Vec<u8>, Vec<Val>)>, Vec<Val>, Option<String>, String) = serde_json::from_str(string.as_str())?;

    run_func(entry.unwrap(), &funcs, &mut stack, 0);

    return Ok(());
}

#[allow(unused)]
pub fn run_func(name: String, funcs: &HashMap<String, (Vec<u8>, Vec<Val>)>, stack: &mut Vec<Val>, d: u8) {
    let mut profTimer: Option<Instant> = None;

    let d = d+1;
    if d > MAX_RECURSION_DEPTH {
        panic!("stack overflow error")
    }

    let (func, consts) = funcs.get(&name).expect(format!("non existent function: {}", name).as_str());
    
    let mut env: Vec<Val>   = vec![];

    let mut i: usize = 0;
    while i < func.len() {
        match func[i] {
            BCInst::PRINT => {
                print_val(stack.pop().expect("missing val on stack for print operation"))
            }
            BCInst::LOAD_CONST => {
                stack.push(consts[func[i+1] as usize].clone());
                i+=1;
            }
            BCInst::ADD => {
                let val2 = stack.pop().expect("not enough vals on stack for add op.");
                let val1 = stack.pop().expect("not enough vals on stack for add op.");

                match val1 {
                    Val::Int(v) => {
                        match val2 {
                            Val::Int(v1) => {
                                stack.push(Val::Int(v.checked_add(v1).expect("addition with overflow")));
                            }

                            Val::Float(v1) => {
                                stack.push(Val::Float(v as f64 + v1));
                            }

                            Val::Long(v1) => {
                                stack.push(Val::Long((v as i64).checked_add(v1).expect("addition with overflow")));
                            }

                            Val::String(mut v1) => {
                                v1.insert_str(0, v.to_string().as_str());
                                stack.push(Val::String(v1));
                            }

                            Val::Bool(_) => {
                                panic!("cannot add bool to int");
                            }

                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot add array to int")
                            }
                        }
                    }

                    Val::Long(v) => {
                        match val2 {
                            Val::Int(v1) => {
                                stack.push(Val::Long(v.checked_add(v1 as i64).expect("addition with overflow")));
                            }

                            Val::Float(v1) => {
                                stack.push(Val::Float(v as f64 + v1));
                            }

                            Val::Long(v1) => {
                                stack.push(Val::Long(v.checked_add(v1).expect("addition with overflow")));
                            }

                            Val::String(mut v1) => {
                                v1.insert_str(0, v.to_string().as_str());
                                stack.push(Val::String(v1));
                            }

                            Val::Bool(_) => {
                                panic!("cannot add bool to long");
                            }

                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot add array to long")
                            }
                        }
                    }

                    Val::Float(v) => {
                        match val2 {
                            Val::Int(v1) => {
                                stack.push(Val::Float(v + v1 as f64));
                            }

                            Val::Float(v1) => {
                                stack.push(Val::Float(v + v1));
                            }

                            Val::Long(v1) => {
                                stack.push(Val::Float(v + v1 as f64));
                            }

                            Val::String(mut v1) => {
                                v1.insert_str(0, v.to_string().as_str());
                                stack.push(Val::String(v1));
                            }

                            Val::Bool(_) => {
                                panic!("cannot add bool to float");
                            }

                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot add array to float")
                            }
                        }
                    }

                    Val::String(v) => {
                        match val2 {
                            Val::String(v1) => {
                                stack.push(Val::String(v + v1.as_str()));
                            }
                            Val::Bool(v1) => {
                                stack.push(Val::String(format!("{}{}", v, v1)));
                            }
                            Val::Float(v1) => {
                                stack.push(Val::String(format!("{}{}", v, v1)));
                            }
                            Val::Int(v1) => {
                                stack.push(Val::String(format!("{}{}", v, v1)));
                            }
                            Val::Long(v1) => {
                                stack.push(Val::String(format!("{}{}", v, v1)));
                            }
                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot add array to string")
                            }
                        }
                    }

                    Val::Bool(_) => {
                        panic!("cannot add bools");
                    }

                    Val::Array{ty: _, arr: _} => {
                        panic!("cannot add arrays");
                    }
                }
            }
            
            BCInst::SUB => {
                let val2 = stack.pop().expect("not enough vals on stack for subtraction op.");
                let val1 = stack.pop().expect("not enough vals on stack for subtraction op.");

                match val1 {
                    Val::Int(v) => {
                        match val2 {
                            Val::Int(v1) => {
                                stack.push(Val::Int(v.checked_sub(v1).expect("subtraction with overflow")));
                            }

                            Val::Float(v1) => {
                                stack.push(Val::Float(v as f64 - v1));
                            }

                            Val::Long(v1) => {
                                stack.push(Val::Long((v as i64).checked_sub(v1).expect("subtraction with overflow")));
                            }

                            Val::String(_) => {
                                panic!("cannot subtract string from int");
                            }

                            Val::Bool(_) => {
                                panic!("cannot subtract bool from int");
                            }

                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot subtract array from int")
                            }
                        }
                    }

                    Val::Long(v) => {
                        match val2 {
                            Val::Int(v1) => {
                                stack.push(Val::Long(v.checked_sub(v1 as i64).expect("subtraction with overflow")));
                            }

                            Val::Float(v1) => {
                                stack.push(Val::Float(v as f64 - v1));
                            }

                            Val::Long(v1) => {
                                stack.push(Val::Long(v.checked_sub(v1).expect("subtraction with overflow")));
                            }

                            Val::String(_) => {
                                panic!("cannot subtract string from long");
                            }

                            Val::Bool(_) => {
                                panic!("cannot subtract bool from long");
                            }

                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot subtract array from long")
                            }
                        }
                    }

                    Val::Float(v) => {
                        match val2 {
                            Val::Int(v1) => {
                                stack.push(Val::Float(v - v1 as f64));
                            }

                            Val::Float(v1) => {
                                stack.push(Val::Float(v - v1));
                            }

                            Val::Long(v1) => {
                                stack.push(Val::Float(v - v1 as f64));
                            }

                            Val::String(mut v1) => {
                                panic!("cannot subtract string from float")
                            }

                            Val::Bool(_) => {
                                panic!("cannot subtract bool from float");
                            }

                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot subtract array from float")
                            }
                        }
                    }

                    Val::String(v) => {
                        panic!("cannot subtract strings");
                    }

                    Val::Bool(_) => {
                        panic!("cannot subtract bools");
                    }

                    Val::Array{ty: _, arr: _} => {
                        panic!("cannot subtract arrays")
                    }
                }
            }

            BCInst::MUL => {
                let val1 = stack.pop().expect("not enough vals on stack for multiplication op.");
                let val2 = stack.pop().expect("not enough vals on stack for multiplication op.");

                match val1 {
                    Val::Int(v) => {
                        match val2 {
                            Val::Int(v1) => {
                                stack.push(Val::Int(v.checked_mul(v1).expect("multiplication with overflow")));
                            }

                            Val::Float(v1) => {
                                stack.push(Val::Float(v as f64 * v1));
                            }

                            Val::Long(v1) => {
                                stack.push(Val::Long((v as i64).checked_mul(v1).expect("multiplication with overflow")));
                            }

                            Val::String(_) => {
                                panic!("cannot multiply string and int");
                            }

                            Val::Bool(_) => {
                                panic!("cannot multiply bool and int");
                            }

                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot multiply array and int")
                            }
                        }
                    }

                    Val::Long(v) => {
                        match val2 {
                            Val::Int(v1) => {
                                stack.push(Val::Long(v.checked_mul(v1 as i64).expect("multiplication with overflow")));
                            }

                            Val::Float(v1) => {
                                stack.push(Val::Float(v as f64 * v1));
                            }

                            Val::Long(v1) => {
                                stack.push(Val::Long(v.checked_mul(v1).expect("multiplication with overflow")));
                            }

                            Val::String(_) => {
                                panic!("cannot multiply string and long");
                            }

                            Val::Bool(_) => {
                                panic!("cannot multiply bool and long");
                            }

                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot multiply array and long")
                            }
                        }
                    }

                    Val::Float(v) => {
                        match val2 {
                            Val::Int(v1) => {
                                stack.push(Val::Float(v * v1 as f64));
                            }

                            Val::Float(v1) => {
                                stack.push(Val::Float(v * v1));
                            }

                            Val::Long(v1) => {
                                stack.push(Val::Float(v * v1 as f64));
                            }

                            Val::String(mut v1) => {
                                panic!("cannot multiply string and float")
                            }

                            Val::Bool(_) => {
                                panic!("cannot multiply bool and float");
                            }

                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot multiply array and float")
                            }
                        }
                    }

                    Val::String(v) => {
                        panic!("cannot multiply strings");
                    }

                    Val::Bool(_) => {
                        panic!("cannot multiply bools");
                    }

                    Val::Array{ty: _, arr: _} => {
                        panic!("cannot multiply arrays")
                    }
                }
            }

            BCInst::DIV => {
                let val2 = stack.pop().expect("not enough vals on stack for div op.");
                let val1 = stack.pop().expect("not enough vals on stack for div op.");

                match val1 {
                    Val::Int(v) => {
                        match val2 {
                            Val::Int(v1) => {
                                stack.push(Val::Int(v.checked_div(v1).expect("divison with overflow")));
                            }

                            Val::Float(v1) => {
                                stack.push(Val::Float(v as f64 / v1));
                            }

                            Val::Long(v1) => {
                                stack.push(Val::Long((v as i64).checked_div(v1).expect("divison with overflow")));
                            }

                            Val::String(_) => {
                                panic!("cannot devide string and int");
                            }

                            Val::Bool(_) => {
                                panic!("cannot devide bool and int");
                            }

                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot devide array and int")
                            }
                        }
                    }

                    Val::Long(v) => {
                        match val2 {
                            Val::Int(v1) => {
                                stack.push(Val::Long(v.checked_div(v1 as i64).expect("divison with overflow")));
                            }

                            Val::Float(v1) => {
                                stack.push(Val::Float(v as f64 / v1));
                            }

                            Val::Long(v1) => {
                                stack.push(Val::Long(v.checked_div(v1).expect("divison with overflow")));
                            }

                            Val::String(_) => {
                                panic!("cannot devide string and long");
                            }

                            Val::Bool(_) => {
                                panic!("cannot devide bool and long");
                            }

                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot devide array and long")
                            }
                        }
                    }

                    Val::Float(v) => {
                        match val2 {
                            Val::Int(v1) => {
                                stack.push(Val::Float(v / v1 as f64));
                            }

                            Val::Float(v1) => {
                                stack.push(Val::Float(v / v1));
                            }

                            Val::Long(v1) => {
                                stack.push(Val::Float(v / v1 as f64));
                            }

                            Val::String(mut v1) => {
                                panic!("cannot devide string and float")
                            }

                            Val::Bool(_) => {
                                panic!("cannot devide bool and float");
                            }

                            Val::Array{ty: _, arr: _} => {
                                panic!("cannot devide array and float")
                            }
                        }
                    }

                    Val::String(v) => {
                        panic!("cannot devide strings");
                    }

                    Val::Bool(_) => {
                        panic!("cannot devide bools");
                    }

                    Val::Array{ty: _, arr: _} => {
                        panic!("cannot devide arrays")
                    }
                }
            }

            BCInst::STORE_LOCAL_VAL => {
                let val = stack.pop().expect("not enough vals");
                let idx = func[i+1] as usize;
                if idx < env.len() {
                    env[idx] = val;
                }
                else if idx == env.len() {
                    env.push(val);
                }
                else {
                    panic!("random access to env not allowed");
                }
                i+=1;
            }

            BCInst::LOAD_LOCAL_VAL => {
                let val = env[func[i+1] as usize].clone();
                stack.push(val);
                i+=1;
                let temp = stack.pop().unwrap();
                stack.push(temp);
            }

            BCInst::CALL_FUNC => {
                if let Val::String(v) = stack.pop().expect("not enough vals") {
                    run_func(v, funcs, stack, d);
                }
                else {
                    panic!("the top stack val has to be a string for CALL_FUNC");
                }
            }

            BCInst::EQUAL => {
                let val2 = stack.pop().expect("not enough vals on stack for equal op.");
                let val1 = stack.pop().expect("not enough vals on stack for equal op.");

                match val1 {
                    Val::Int(v) => {
                        if let Val::Int(v1) = val2 {
                            stack.push(Val::Bool(v == v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::Long(v) => {
                        if let Val::Long(v1) = val2 {
                            stack.push(Val::Bool(v == v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::Float(v) => {
                        if let Val::Float(v1) = val2 {
                            stack.push(Val::Bool(v == v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::String(v) => {
                        if let Val::String(v1) = val2 {
                            stack.push(Val::Bool(v == v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::Bool(v) => {
                        if let Val::Bool(v1) = val2 {
                            stack.push(Val::Bool(v == v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::Array{ty: _, arr} => {
                        if let Val::Array{ty: _, arr: arr1} = val2 {
                            stack.push(Val::Bool(arr == arr1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }
                }
            }

            BCInst::GREATER_THAN => {
                let val2 = stack.pop().expect("not enough vals on stack for greater than op.");
                let val1 = stack.pop().expect("not enough vals on stack for greater than op.");

                match val1 {
                    Val::Int(v) => {
                        if let Val::Int(v1) = val2 {
                            stack.push(Val::Bool(v > v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::Long(v) => {
                        if let Val::Long(v1) = val2 {
                            stack.push(Val::Bool(v > v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::Float(v) => {
                        if let Val::Float(v1) = val2 {
                            stack.push(Val::Bool(v > v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::String(_) => {
                        panic!("cannot compare strings");
                    }

                    Val::Bool(_) => {
                        panic!("cannot compare bools");
                    }

                    Val::Array{ty: _, arr: _} => {
                        panic!("cannot compare arrays");
                    }
                }
            }

            BCInst::LESS_THAN => {
                let val2 = stack.pop().expect("not enough vals on stack for less than op.");
                let val1 = stack.pop().expect("not enough vals on stack for less than op.");

                match val1 {
                    Val::Int(v) => {
                        if let Val::Int(v1) = val2 {
                            stack.push(Val::Bool(v < v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::Long(v) => {
                        if let Val::Long(v1) = val2 {
                            stack.push(Val::Bool(v < v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::Float(v) => {
                        if let Val::Float(v1) = val2 {
                            stack.push(Val::Bool(v < v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::String(_) => {
                        panic!("cannot compare strings");
                    }

                    Val::Bool(_) => {
                        panic!("cannot compare bools");
                    }

                    Val::Array{ty: _, arr: _} => {
                        panic!("cannot compare arrays");
                    }
                }
            }

            BCInst::GREATER_THAN_EQUAL => {
                let val2 = stack.pop().expect("not enough vals on stack for greater than op.");
                let val1 = stack.pop().expect("not enough vals on stack for greater than op.");

                match val1 {
                    Val::Int(v) => {
                        if let Val::Int(v1) = val2 {
                            stack.push(Val::Bool(v >= v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::Long(v) => {
                        if let Val::Long(v1) = val2 {
                            stack.push(Val::Bool(v >= v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::Float(v) => {
                        if let Val::Float(v1) = val2 {
                            stack.push(Val::Bool(v >= v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::String(_) => {
                        panic!("cannot compare strings");
                    }

                    Val::Bool(_) => {
                        panic!("cannot compare bools");
                    }

                    Val::Array{ty: _, arr: _} => {
                        panic!("cannot compare arrays");
                    }
                }
            }

            BCInst::LESS_THAN_EQUAL => {
                let val2 = stack.pop().expect("not enough vals on stack for greater than op.");
                let val1 = stack.pop().expect("not enough vals on stack for greater than op.");

                match val1 {
                    Val::Int(v) => {
                        if let Val::Int(v1) = val2 {
                            stack.push(Val::Bool(v <= v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::Long(v) => {
                        if let Val::Long(v1) = val2 {
                            stack.push(Val::Bool(v <= v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::Float(v) => {
                        if let Val::Float(v1) = val2 {
                            stack.push(Val::Bool(v <= v1));
                        }
                        else {
                            panic!("can only add the same types");
                        }
                    }

                    Val::String(_) => {
                        panic!("cannot compare strings");
                    }

                    Val::Bool(_) => {
                        panic!("cannot compare bools");
                    }

                    Val::Array{ty: _, arr: _} => {
                        panic!("cannot compare arrays");
                    }
                }
            }

            BCInst::OR => {
                if let Val::Bool(v) = stack.pop().expect("stack too short") {
                    if let Val::Bool(v1) = stack.pop().expect("stack too short") {
                        stack.push(Val::Bool(v || v1));
                    }
                }
                else {
                    panic!("top val has to be bool!");
                }
            }

            BCInst::AND => {
                if let Val::Bool(v) = stack.pop().expect("stack too short") {
                    if let Val::Bool(v1) = stack.pop().expect("stack too short") {
                        stack.push(Val::Bool(v && v1));
                    }
                }
                else {
                    panic!("top val has to be bool!");
                }
            }

            BCInst::NOT => {
                if let Val::Bool(v) = stack.pop().expect("stack too short") {
                    stack.push(Val::Bool(!v));
                }
                else {
                    panic!("top val has to be bool!");
                }
            }

            BCInst::JUMP_IF_TRUE => {
                if let Val::Bool(v) = stack.pop().expect("stack too short") {
                    let (dist, len, go_fwd) = get_jump_dist(&func, i);
                    if v {
                        if go_fwd {
                            i += dist;
                        }
                        else {
                            i-= dist;
                        }
                    }
                    else {
                        i+= len+1;
                    }
                }
                else {
                    panic!("top val has to be bool!");
                } 
            }

            BCInst::JUMP_IF_FALSE => {
                if let Val::Bool(v) = stack.pop().expect("stack too short") {
                    let (dist, len, go_fwd) = get_jump_dist(&func, i);
                    if !v {
                        if go_fwd {
                            i += dist+1;
                        }
                        else {
                            i-= dist+1;
                        }
                    }
                    else {
                        i+= len+1;
                    }
                }
                else {
                    panic!("top val has to be bool!");
                }
            }

            BCInst::JUMP => {
                let (dist, _, go_fwd) = get_jump_dist(&func, i);

                if go_fwd {
                    i += dist;
                }
                else {
                    i -= dist;
                }
            }

            BCInst::INSERT_ARR_AT => {
                let nval = stack.pop().expect("stack too short");
                let index = stack.pop().expect("stack too short");
                let array = stack.pop().expect("stack too short");

                if let Val::Array{ty, mut arr} = array {
                    match index {
                        Val::Int(i) => {
                            if (i as usize) < arr.len() {
                                match nval {
                                    Val::Array{ty: _, arr: _} => {
                                        if ty == Type::Array {
                                            arr[i as usize] = nval;
                                        }
                                        else {
                                            panic!("The type of the given value does not match the one required for the array")
                                        }
                                    }
                                    Val::Int(_) => {
                                        if ty == Type::Int {
                                            arr[i as usize] = nval;
                                        }
                                        else {
                                            panic!("The type of the given value does not match the one required for the array")
                                        }
                                    }
                                    Val::Float(_) => {
                                        if ty == Type::Float {
                                            arr[i as usize] = nval;
                                        }
                                        else {
                                            panic!("The type of the given value does not match the one required for the array")
                                        }
                                    }
                                    Val::Long(_) => {
                                        if ty == Type::Long {
                                            arr[i as usize] = nval;
                                        }
                                        else {
                                            panic!("The type of the given value does not match the one required for the array")
                                        }
                                    }
                                    Val::String(_) => {
                                        if ty == Type::String {
                                            arr[i as usize] = nval;
                                        }
                                        else {
                                            panic!("The type of the given value does not match the one required for the array")
                                        }
                                    }
                                    Val::Bool(_) => {
                                        if ty == Type::Bool {
                                            arr[i as usize] = nval;
                                        }
                                        else {
                                            panic!("The type of the given value does not match the one required for the array")
                                        }
                                    }
                                }
                            }
                        }

                        _ => {
                            panic!("can only index array using int");
                        }
                    }
                }
            }

            BCInst::GET_ARR_AT => {
                let idx = stack.pop().expect("stack too short");
                let array = stack.pop().expect("stack too short");

                if let Val::Array{ty, arr} = array {
                    if let Val::Int(i) = idx {
                        stack.push(arr[i as usize].clone());
                    }
                    else {
                        panic!("arrays can only be indexed by integers");
                    }
                }
                else {
                    panic!("The second to top value has to be an array");
                }
            }

            BCInst::PUSH_TO_ARR => {
                let val = stack.pop().expect("stack too short");
                let array = stack.pop().expect("stack too short");

                if let Val::Array{ty, mut arr} = array {
                    arr.push(val);
                    stack.push(Val::Array{ty, arr});
                }
                else {
                    panic!("the second to top value has to be an array");
                }
            }

            BCInst::POP_FROM_ARR => {
                let array = stack.pop().expect("stack too short");
                if let Val::Array{ty, mut arr} = array {
                    stack.push(arr.pop().expect("the array could not be poped"));
                    stack.push(Val::Array{ty, arr});
                }
            }

            BCInst::LOAD_ARR_LEN => {
                let array = stack.pop().expect("stack too short");
                if let Val::Array{ty, arr} = array {
                    stack.push(Val::Int(arr.len() as i32));
                }
                else {
                    panic!("can only index arrays");
                }
            }

            BCInst::MAKE_ARR => {
                let len = stack.pop().expect("stack too short");
                let ty = stack.pop().expect("stack too short");

                if let Val::Int(i) = len {
                    let mut res = vec![];
                    for j in 0..i {
                        res.push(stack.pop().expect("stack too short for specified array length"));
                    }

                    stack.push(Val::Array{ty: Type::Int, arr: res});
                }
                else {
                    panic!("the top value hast to be a int for array creation");
                }
            }

            BCInst::START_PROF => {
                if profTimer.is_some() {panic!("can not profile more than once at the same time")};
                profTimer = Some(Instant::now());
            }

            BCInst::STOP_PROF => {
                if profTimer.is_none() {panic!("can only stop a profile if there is one currently running")};

                println!("PROFILER: The time since profile start: {:?}", profTimer.unwrap());
            }

            BCInst::NOP => {}

            BCInst::RET =>{
                return;
            }

            _ => panic!("unknown byte code: {}", func[i])
        }
        i+=1;
    }
}

fn get_jump_dist(func: &Vec<u8>, i: usize) -> (usize, usize, bool) {
    let a = ((func[i+1]) & (240))>>4;
    let b = (func[i+1]) & (15);
    if a == 0 {
        match b {
            1 => {return ((func[i+2] + 1) as usize, b as usize, true)}

            2 => {
                let inc = ((func[i+2] as u16)<<8) | func[i+3] as u16;
                return ((inc + 1) as usize, b as usize, true);
            }

            4 => {
                let inc = ((func[i+2] as u32)<<26) | ((func[i+3] as u32)<<16) | ((func[i+4] as u32)<<8) | (func[i+5] as u32);
                return ((inc + 1) as usize, b as usize, true);
            }

            8 => {
                let mut inc: u64 = 0;
                for j in 2..10 {
                    inc = inc | (func[j] as u64)<<(64-(8*(j-1)));
                }
                return ((inc + 1) as usize, b as usize, true);
            }

            _=>{panic!("incorrect input to jump function")}
        }
    }
    else if a == 1 {
        match b {
            1 => {return ((func[i+2] + 1) as usize, b as usize, false)}

            2 => {
                let inc = ((func[i+2] as u16)<<8) | func[i+3] as u16;
                return ((inc + 1) as usize, b as usize, false);
            }

            4 => {
                let inc = ((func[i+2] as u32)<<26) | ((func[i+3] as u32)<<16) | ((func[i+4] as u32)<<8) | (func[i+5] as u32);
                return ((inc + 1) as usize, b as usize, false);
            }

            8 => {
                let mut inc: u64 = 0;
                for j in 2..10 {
                    inc = inc | (func[j] as u64)<<(64-(8*(j-1)));
                }
                return ((inc + 1) as usize, b as usize, false);
            }

            _=>{panic!("incorrect input to jump function")}
        }
    }
    else {panic!("incorrect input to jump function")}
}