//! The runtime state of a method that is currently executing in the virtual machine.
//!
//! Each thread in the Java virtual machine has a _runtime stack_ comprised of _frames_. A new
//! frame is pushed onto the stack each time a method is invoked. Frames are removed from the stack
//! either when the method of the current stack frame returns, or when an unhandled exception
//! causes abrupt completion of the method (causing stack unwinding).
//!
//! Every stack frame has an array of bytecode instructions and a _program counter_ which define
//! its execution. It also maintains a fixed number of _local variables_ and an _operand stack_.
//! The operand stack is the central focus of the Java machine bytecode, and is directly
//! manipulated by the bytecode instructions (in lieu of registers).

use std::cell::RefCell;
use std::num::Wrapping;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub};
use std::rc::Rc;

use model::class_file::access_flags::class_access_flags;

use vm::{sig, symref};
use vm::bytecode::opcode;
use vm::class::Class;
use vm::class_loader::ClassLoader;
use vm::constant_pool::RuntimeConstantPoolEntry;
use vm::sig::Type;
use vm::value::{Array, Scalar, Value};

/// A frame is used to store data and partial results, as well as to perform dynamic linking,
/// return values for methods, and dispatch exceptions.
#[derive(Debug)]
pub struct Frame<'a> {
    /// A reference to the class containing the currently executing method.
    current_class: &'a Class,
    /// The bytecode currently executing in this frame.
    code: &'a [u8],
    /// The current program counter.
    pc: u16,
    /// The local variables of the current method.
    /// Values that occupy two indices (`long` and `double`) are stored in one slot followed by a
    /// `None` value in the subsequent index.
    local_variables: Vec<Option<Value>>,
    /// The operand stack manipulated by the instructions of the current method.
    operand_stack: Vec<Value>,
}

impl<'a> Frame<'a> {
    pub fn new(current_class: &'a Class, code: &'a [u8],
               local_variables: Vec<Option<Value>>) -> Self {
        Frame {
            current_class: current_class,
            code: code,
            pc: 0,
            local_variables: local_variables,
            operand_stack: vec![],
        }
    }

    /// Read a byte (`u8`) value and advance the program counter.
    fn read_next_byte(&mut self) -> u8 {
        let result = self.code[self.pc as usize];
        self.pc += 1;
        result
    }

    /// Read a short (`u16`) value and advance the program counter by 2.
    fn read_next_short(&mut self) -> u16 {
        ((self.read_next_byte() as u16) << 8) | (self.read_next_byte() as u16)
    }

    /// Remove `count` items from the operand stack.
    fn pop_multi(&mut self, count: usize) -> Vec<Value> {
        let start_index = self.operand_stack.len() - count;
        self.operand_stack.drain(start_index..).collect()
    }

    /// Execute the method associated with this stack frame in the context of the currrent class
    /// loader, and return a result if there is one. This method may create new stack frames as a
    /// result of evaluating `invoke*` instructions.
    pub fn run(mut self, class_loader: &mut ClassLoader) -> Option<Value> {
        macro_rules! pop {
            () => (self.operand_stack.pop().unwrap());
            ($value_variant: path) => ({
                match pop!() {
                    $value_variant(v) => v,
                    v => panic!("Expected to pop a value of type {}, but was {:?}",
                                stringify!($value_variant), v),
                }
            });
        }

        macro_rules! pop_not_null {
            () => ({
                match pop!() {
                    Value::NullReference => panic!(
                        "NullPointerException: expected {} but was null",
                        stringify!($value_variant)),
                    v => v,
                }
            });
            ($value_variant: path) => ({
                match pop!() {
                    Value::NullReference => panic!(
                        "NullPointerException: expected {} but was null",
                        stringify!($value_variant)),
                    $value_variant(v) => v,
                    v => panic!("Expected to pop a value of type {}, but was {:?}",
                                stringify!($value_variant), v),
                }
            });
        }

        macro_rules! push {
            ($v: expr) => ({
                let v = $v;     // satisfy the borrow checker
                self.operand_stack.push(v);
            });
            ($($vs: expr),*) => ({
                self.operand_stack.extend_from_slice(&[$($vs),*])
            })
        }

        macro_rules! with {
            ($read_next_action: ident, $k: ident) => ({
                let value = self.$read_next_action() as u16;
                $k!(value);
            })
        }

        macro_rules! do_ipush {
            ($value: ident) => (push!(Value::Int(Wrapping($value as i32))))
        }

        macro_rules! do_ldc {
            ($index: ident) => ({
                let value = self.current_class.get_constant_pool()
                    .resolve_literal($index, class_loader).unwrap();
                push!(value);
            });
        }

        macro_rules! do_load {
            ($index: expr) => ({
                let value = self.local_variables[$index as usize].clone().unwrap();
                push!(value);
            })
        }

        macro_rules! do_store {
            ($index: expr) => ({
                let value = self.operand_stack.pop().unwrap();
                // invalidate the slot after this one if we're storing a category 2 operand
                match value {
                    Value::Int(_) | Value::Float(_) | Value::ScalarReference(_)
                            | Value::ArrayReference(_) | Value::NullReference => (),
                    Value::Long(_) | Value::Double(_) => {
                        self.local_variables[($index + 1) as usize] = None;
                    },
                }
                // actually store the local variable
                self.local_variables[$index as usize] = Some(value);
                // invalidate the slot before this one if it was formerly storing a category 2
                // operand
                let prev_index = $index - 1;
                if prev_index > 0 {
                    match self.local_variables[prev_index as usize] {
                        None | Some(Value::Int(_)) | Some(Value::Float(_))
                                | Some(Value::ScalarReference(_)) | Some(Value::ArrayReference(_))
                                | Some(Value::NullReference) => (),
                        Some(Value::Long(_)) | Some(Value::Double(_)) => {
                            self.local_variables[prev_index as usize] = None;
                        },
                    }
                }
            })
        }

        macro_rules! do_binop {
            ($value_variant: path, $binop: expr) => ({
                let v2 = pop!($value_variant);
                let v1 = pop!($value_variant);
                push!($value_variant($binop(v1, v2)));
            });
        }

        macro_rules! do_if_icmp {
            ($cmp_op: expr) => ({
                let branch_offset = self.read_next_short() as i16;
                let i2 = pop!(Value::Int);
                let i1 = pop!(Value::Int);
                if $cmp_op(&i1, &i2) {
                    // 3 byte long instruction; read* operations move the PC.
                    let this_pc_start = self.pc - 3;
                    self.pc = (this_pc_start as i32 + branch_offset as i32) as u16
                }
            });
        }

        macro_rules! do_if_acmp {
            ($cmp_op: expr) => ({
                let branch_offset = self.read_next_short() as i16;
                let v2 = pop!();
                let v1 = pop!();
                match (v1, v2) {
                    (Value::ArrayReference(x), Value::ArrayReference(y)) => {
                        if x.as_ref() as *const uint == y.as_ref() as *const uint {
                            // 3 byte long instruction; read* operations move the PC.
                            let this_pc_start = self.pc - 3;
                            self.pc = (this_pc_start as i32 + branch_offset as i32) as u16
                        }
                    },
                    (Value::ScalarReference(x), Value::ScalarReference(y)) => {
                        if x.as_ref() as *const uint == y.as_ref() as *const uint {
                            // 3 byte long instruction; read* operations move the PC.
                            let this_pc_start = self.pc - 3;
                            self.pc = (this_pc_start as i32 + branch_offset as i32) as u16
                        }
                    },
                    _ => ()
                }
            });
        }

        macro_rules! do_if_int {
            ($pred: expr) => ({
                let branch_offset = self.read_next_short() as i16;
                let x = pop!(Value::Int);
                if $pred(x) {
                    // 3 byte long instruction; read* operations move the PC.
                    let this_pc_start = self.pc - 3;
                    self.pc = (this_pc_start as i32 + branch_offset as i32) as u16
                }
            });
        }

        loop {
            match self.read_next_byte() {
                opcode::NOP => (),
                opcode::ACONST_NULL => push!(Value::NullReference),
                opcode::ICONST_M1 => push!(Value::Int(Wrapping(-1))),
                opcode::ICONST_0 => push!(Value::Int(Wrapping(0))),
                opcode::ICONST_1 => push!(Value::Int(Wrapping(1))),
                opcode::ICONST_2 => push!(Value::Int(Wrapping(2))),
                opcode::ICONST_3 => push!(Value::Int(Wrapping(3))),
                opcode::ICONST_4 => push!(Value::Int(Wrapping(4))),
                opcode::ICONST_5 => push!(Value::Int(Wrapping(5))),
                opcode::LCONST_0 => push!(Value::Long(Wrapping(0))),
                opcode::LCONST_1 => push!(Value::Long(Wrapping(1))),
                opcode::FCONST_0 => push!(Value::Float(0.0)),
                opcode::FCONST_1 => push!(Value::Float(1.0)),
                opcode::FCONST_2 => push!(Value::Float(2.0)),
                opcode::DCONST_0 => push!(Value::Double(0.0)),
                opcode::DCONST_1 => push!(Value::Double(1.0)),
                opcode::BIPUSH => with!(read_next_byte, do_ipush),
                opcode::SIPUSH => with!(read_next_short, do_ipush),
                opcode::LDC => with!(read_next_byte, do_ldc),
                opcode::LDC_W | opcode::LDC2_W => with!(read_next_short, do_ldc),

                // these are a little out of order, since we combine identical cases
                opcode::ILOAD | opcode::LLOAD | opcode::FLOAD | opcode::DLOAD | opcode::ALOAD =>
                    with!(read_next_byte, do_load),
                opcode::ILOAD_0 | opcode::LLOAD_0 | opcode::FLOAD_0 | opcode::DLOAD_0
                        | opcode::ALOAD_0 =>
                    do_load!(0),
                opcode::ILOAD_1 | opcode::LLOAD_1 | opcode::FLOAD_1 | opcode::DLOAD_1
                        | opcode::ALOAD_1 =>
                    do_load!(1),
                opcode::ILOAD_2 | opcode::LLOAD_2 | opcode::FLOAD_2 | opcode::DLOAD_2
                        | opcode::ALOAD_2 =>
                    do_load!(2),
                opcode::ILOAD_3 | opcode::LLOAD_3 | opcode::FLOAD_3 | opcode::DLOAD_3
                        | opcode::ALOAD_3 =>
                    do_load!(3),
                opcode::IALOAD | opcode::LALOAD | opcode::FALOAD | opcode::DALOAD
                        | opcode::AALOAD | opcode::BALOAD | opcode::CALOAD | opcode::SALOAD => {
                    let Wrapping(index) = pop!(Value::Int);
                    let array_rc = pop_not_null!(Value::ArrayReference);
                    push!(array_rc.borrow_mut().get(index));
                },

                // same thing here
                opcode::ISTORE | opcode::LSTORE | opcode::FSTORE | opcode::DSTORE | opcode::ASTORE =>
                    with!(read_next_byte, do_store),
                opcode::ISTORE_0 | opcode::LSTORE_0 | opcode::FSTORE_0 | opcode::DSTORE_0
                        | opcode::ASTORE_0 =>
                    do_store!(0),
                opcode::ISTORE_1 | opcode::LSTORE_1 | opcode::FSTORE_1 | opcode::DSTORE_1
                        | opcode::ASTORE_1 =>
                    do_store!(1),
                opcode::ISTORE_2 | opcode::LSTORE_2 | opcode::FSTORE_2 | opcode::DSTORE_2
                        | opcode::ASTORE_2 =>
                    do_store!(2),
                opcode::ISTORE_3 | opcode::LSTORE_3 | opcode::FSTORE_3 | opcode::DSTORE_3
                        | opcode::ASTORE_3 =>
                    do_store!(3),
                opcode::IASTORE | opcode::LASTORE | opcode::FASTORE | opcode::DASTORE | opcode::AASTORE | opcode::BASTORE | opcode::CASTORE | opcode::SASTORE => {
                    let value = pop!();
                    let Wrapping(index) = pop!(Value::Int);
                    let array_rc = pop_not_null!(Value::ArrayReference);
                    array_rc.borrow_mut().put(index, value);
                },

                opcode::POP => {
                    pop!();
                },

                opcode::POP2 => {
                    match pop!() {
                        Value::Long(_) | Value::Double(_) => (),
                        _ => {
                            pop!();
                        }
                    }
                },
                opcode::DUP => {
                    // TODO make this a macro
                    let value = self.operand_stack.last().unwrap().clone();
                    push!(value);
                },
                opcode::DUP_X1 => {
                    let value1 = pop!();
                    let value2 = pop!();

                    // TODO: make this a macro
                    push!(value1.clone(), value2, value1);
                },
                opcode::DUP_X2 => {
                    let value1 = pop!();
                    let value2 = pop!();
                    match value2 {
                        Value::Long(_) | Value::Double(_) => {
                            push!(value1.clone(), value2, value1);
                        },
                        _ => {
                            let value3 = pop!();
                            push!(value1.clone(), value3, value2, value1);
                        },
                    }
                },
                opcode::DUP2 => {
                    let value1 = pop!();
                    match value1 {
                        Value::Long(_) | Value::Double(_) => {
                            push!(value1.clone(), value1);
                        },
                        _ => {
                            let value2 = pop!();
                            push!(value2.clone(), value1.clone(), value2, value1);
                        },
                    }
                },

                opcode::SWAP => {
                    // both values need to be category 1
                    let v1 = pop!();
                    let v2 = pop!();
                    push!(v1);
                    push!(v2);
                },

                opcode::IADD => do_binop!(Value::Int, Wrapping::<i32>::add),
                opcode::LADD => do_binop!(Value::Long, Wrapping::<i64>::add),
                opcode::FADD => do_binop!(Value::Float, f32::add),
                opcode::DADD => do_binop!(Value::Double, f64::add),
                opcode::ISUB => do_binop!(Value::Int, Wrapping::<i32>::sub),
                opcode::LSUB => do_binop!(Value::Long, Wrapping::<i64>::sub),
                opcode::FSUB => do_binop!(Value::Float, f32::sub),
                opcode::DSUB => do_binop!(Value::Double, f64::sub),
                opcode::IMUL => do_binop!(Value::Int, Wrapping::<i32>::mul),
                opcode::LMUL => do_binop!(Value::Long, Wrapping::<i64>::mul),
                opcode::FMUL => do_binop!(Value::Float, f32::mul),
                opcode::DMUL => do_binop!(Value::Double, f64::mul),
                opcode::IDIV => do_binop!(Value::Int, Wrapping::<i32>::div),
                opcode::LDIV => do_binop!(Value::Long, Wrapping::<i64>::div),
                opcode::FDIV => do_binop!(Value::Float, f32::div),
                opcode::DDIV => do_binop!(Value::Double, f64::div),
                opcode::IREM => do_binop!(Value::Int, Wrapping::<i32>::rem),
                opcode::LREM => do_binop!(Value::Long, Wrapping::<i64>::rem),
                opcode::FREM => do_binop!(Value::Float, f32::rem),
                opcode::DREM => do_binop!(Value::Double, f64::rem),
                // Issue #33037: Neg is missing for Wrapping
                opcode::INEG => push!(Value::Int(!pop!(Value::Int) + Wrapping(1))),
                opcode::LNEG => push!(Value::Long(!pop!(Value::Long) + Wrapping(1))),
                opcode::FNEG => push!(Value::Float(-pop!(Value::Float))),
                opcode::DNEG => push!(Value::Double(-pop!(Value::Double))),
                opcode::ISHL => {
                    let Wrapping(s) = pop!(Value::Int);
                    let v = pop!(Value::Int);
                    push!(Value::Int(v << (s & 0x1F) as usize));
                }
                opcode::LSHL => {
                    let Wrapping(s) = pop!(Value::Int);
                    let v = pop!(Value::Long);
                    push!(Value::Long(v << (s & 0x3F) as usize));
                }
                opcode::ISHR => {
                    let Wrapping(s) = pop!(Value::Int);
                    let v = pop!(Value::Int);
                    push!(Value::Int(v >> (s & 0x1F) as usize));
                }
                opcode::LSHR => {
                    let Wrapping(s) = pop!(Value::Int);
                    let v = pop!(Value::Long);
                    push!(Value::Long(v >> (s & 0x3F) as usize));
                }
                opcode::IUSHR => {
                    let s = (pop!(Value::Int).0 & 0x1F) as usize;
                    let v = pop!(Value::Int).0 as u32;
                    push!(Value::Int(Wrapping((v >> s) as i32)))
                },
                opcode::LUSHR => {
                    let s = (pop!(Value::Int).0 & 0x3F) as usize;
                    let v = pop!(Value::Long).0 as u64;
                    push!(Value::Long(Wrapping((v >> s) as i64)))
                },
                opcode::IAND => do_binop!(Value::Int, Wrapping::<i32>::bitand),
                opcode::LAND => do_binop!(Value::Long, Wrapping::<i64>::bitand),
                opcode::IOR => do_binop!(Value::Int, Wrapping::<i32>::bitor),
                opcode::LOR => do_binop!(Value::Long, Wrapping::<i64>::bitor),
                opcode::IXOR => do_binop!(Value::Int, Wrapping::<i32>::bitxor),
                opcode::LXOR => do_binop!(Value::Long, Wrapping::<i64>::bitxor),
                opcode::IINC => {
                    let index = self.read_next_byte();
                    let c = self.read_next_byte() as i8 as i32;
                    match self.local_variables[index as usize] {
                        Some(Value::Int(ref mut v)) => *v += Wrapping(c),
                        Some(ref v) => panic!("IINC: Expected an int, but was {:?}", v),
                        None => panic!("IINC: Not a local variable at index {}", index),
                    }
                },

                opcode::IF_ICMPEQ => do_if_icmp!(Wrapping::<i32>::eq),
                opcode::IF_ICMPNE => do_if_icmp!(Wrapping::<i32>::ne),
                opcode::IF_ICMPLT => do_if_icmp!(Wrapping::<i32>::lt),
                opcode::IF_ICMPGT => do_if_icmp!(Wrapping::<i32>::gt),
                opcode::IF_ICMPGE => do_if_icmp!(Wrapping::<i32>::ge),
                opcode::IF_ICMPLE => do_if_icmp!(Wrapping::<i32>::le),

                opcode::GOTO => {
                    let branch_offset = self.read_next_short() as i16;
                    let this_pc_start = self.pc - 3;
                    self.pc = (this_pc_start as i32 + branch_offset as i32) as u16;
                },

                opcode::IRETURN | opcode::LRETURN | opcode::FRETURN | opcode::DRETURN
                        | opcode::ARETURN => return self.operand_stack.pop(),
                opcode::RETURN => return None,

                opcode::GETSTATIC => {
                    let index = self.read_next_short();
                    if let Some(RuntimeConstantPoolEntry::FieldRef(ref symref)) =
                            self.current_class.get_constant_pool()[index] {
                        let resolved_class = class_loader.resolve_class(&symref.class).unwrap();
                        let value = resolved_class.resolve_and_get_field(symref, class_loader);
                        push!(value)
                    } else {
                        panic!("getstatic refers to non-field in constant pool");
                    }
                },

                opcode::PUTSTATIC => {
                    let index = self.read_next_short();
                    if let Some(RuntimeConstantPoolEntry::FieldRef(ref symref)) =
                            self.current_class.get_constant_pool()[index] {
                        let resolved_class = class_loader.resolve_class(&symref.class).unwrap();
                        let new_value = pop!();
                        resolved_class.resolve_and_put_field(symref, new_value, class_loader);
                    } else {
                        panic!("putstatic refers to non-field in constant pool");
                    }
                },

                opcode::GETFIELD => {
                    let index = self.read_next_short();
                    if let Some(RuntimeConstantPoolEntry::FieldRef(ref symref)) =
                            self.current_class.get_constant_pool()[index] {
                        match pop!() {
                            Value::ScalarReference(object_rc) => {
                                let value = object_rc.borrow().get_field(&symref.sig).clone();
                                push!(value);
                            },
                            Value::ArrayReference(_) => panic!("getfield called on array"),
                            Value::NullReference => panic!("NullPointerException"),
                            _ => panic!("getfield called on a primitive value"),
                        }
                    } else {
                        panic!("getfield refers to non-field in constant pool");
                    }
                },

                opcode::PUTFIELD => {
                    let index = self.read_next_short();
                    let value = pop!();
                    if let Some(RuntimeConstantPoolEntry::FieldRef(ref symref)) =
                            self.current_class.get_constant_pool()[index] {
                        match pop!() {
                            Value::ScalarReference(object_rc) => {
                                object_rc.borrow_mut().put_field(symref.sig.clone(), value);
                            },
                            Value::ArrayReference(_) => panic!("putfield called on array"),
                            Value::NullReference => panic!("NullPointerException"),
                            _ => panic!("putfield called on a primitive value"),
                        }
                    } else {
                        panic!("putfield refers to non-field in constant pool");
                    }
                },

                opcode::INVOKEVIRTUAL => {
                    let index = self.read_next_short();
                    if let Some(RuntimeConstantPoolEntry::MethodRef(ref symref)) =
                            self.current_class.get_constant_pool()[index] {
                        // TODO: this should throw Java exceptions instead of unwrapping
                        let resolved_class = class_loader.resolve_class(&symref.class).unwrap();
                        let resolved_method = resolved_class.resolve_method(symref);
                        // TODO: check for <clinit> and <init>
                        // TODO: check protected accesses
                        let num_args = symref.sig.params.len();
                        let args = self.pop_multi(num_args + 1);
                        let object_class = {
                            let object_value = &args[0];
                            match *object_value {
                                Value::ScalarReference(ref scalar_rc) =>
                                    scalar_rc.borrow().get_class().clone(),
                                Value::ArrayReference(ref array_rc) =>
                                    array_rc.borrow().get_class().clone(),
                                Value::NullReference => panic!("NullPointerException"),
                                _ => panic!("invokevirtual on a primitive type"),
                            }
                        };
                        match object_class.dispatch_method(resolved_method) {
                            None => panic!("AbstractMethodError"),
                            Some((actual_class, actual_method)) => {
                                let result = actual_method.invoke(actual_class, class_loader, args);
                                match result {
                                    None => (),
                                    Some(value) => self.operand_stack.push(value),
                                }
                            },
                        }
                    } else {
                        panic!("invokevirtual refers to non-method in constant pool");
                    }
                },

                opcode::INVOKESPECIAL => {
                    let index = self.read_next_short();
                    if let Some(RuntimeConstantPoolEntry::MethodRef(ref symref)) =
                            self.current_class.get_constant_pool()[index] {
                        // TODO: this should throw Java exceptions instead of unwrapping
                        let resolved_class = class_loader.resolve_class(&symref.class).unwrap();
                        let resolved_method = resolved_class.resolve_method(symref);
                        // TODO: check protected accesses
                        // TODO: lots of other checks here too
                        let num_args = symref.sig.params.len();
                        let args = self.pop_multi(num_args + 1);

                        // check the three conditions from the spec
                        let actual_method = {
                            if resolved_class.access_flags & class_access_flags::ACC_SUPER == 0
                                    || !self.current_class.is_descendant(resolved_class.as_ref())
                                    || resolved_method.symref.sig.name == "<init>" {
                                resolved_method
                            } else {
                                self.current_class.superclass.as_ref().and_then(|superclass| {
                                    superclass.find_method(&symref.sig)
                                }).expect("AbstractMethodError")
                            }
                        };
                        let actual_class = class_loader.resolve_class(&actual_method.symref.class).unwrap();
                        let result = actual_method.invoke(actual_class.as_ref(), class_loader,
                                args);
                        match result {
                            None => (),
                            Some(value) => self.operand_stack.push(value),
                        }
                    } else {
                        panic!("invokespecial refers to non-method in constant pool");
                    }
                },

                opcode::INVOKESTATIC => {
                    let index = self.read_next_short();
                    if let Some(RuntimeConstantPoolEntry::MethodRef(ref symref)) =
                            self.current_class.get_constant_pool()[index] {
                        // TODO: this should throw Java exceptions instead of unwrapping
                        let resolved_class = class_loader.resolve_class(&symref.class).unwrap();
                        let resolved_method = resolved_class.resolve_method(symref);
                        // TODO: check protected accesses
                        // TODO: lots of other checks here too
                        let num_args = symref.sig.params.len();
                        let args = self.pop_multi(num_args);
                        let result = resolved_method.invoke(resolved_class.as_ref(), class_loader,
                                args);
                        match result {
                            None => (),
                            Some(value) => self.operand_stack.push(value),
                        }
                    } else {
                        panic!("invokestatic refers to non-method in constant pool");
                    }
                },

                opcode::NEW => {
                    let index = self.read_next_short();
                    if let Some(RuntimeConstantPoolEntry::ClassRef(ref symref)) =
                            self.current_class.get_constant_pool()[index] {
                        // TODO check for interfaces, abstract classes
                        let resolved_class = class_loader.resolve_class(symref).unwrap();
                        resolved_class.initialize(class_loader);
                        let object = Scalar::new(resolved_class);
                        let object_rc = Rc::new(RefCell::new(object));
                        push!(Value::ScalarReference(object_rc));
                    } else {
                        panic!("new refers to non-class in constant pool");
                    }
                },

                opcode::NEWARRAY => {
                    let type_tag = self.read_next_byte();
                    let component_ty = match type_tag {
                        4 => Type::Boolean,
                        5 => Type::Char,
                        6 => Type::Float,
                        7 => Type::Double,
                        8 => Type::Byte,
                        9 => Type::Short,
                        10 => Type::Int,
                        11 => Type::Long,
                        _ => panic!("newarray: bad type tag"),
                    };
                    let class_sig = sig::Class::Array(Box::new(component_ty));
                    let class_symref = symref::Class { sig: class_sig };
                    let class = class_loader.resolve_class(&class_symref).unwrap();

                    match pop!() {
                        Value::Int(Wrapping(length)) => {
                            let array = Array::new(class, length);
                            let array_rc = Rc::new(RefCell::new(array));
                            push!(Value::ArrayReference(array_rc));
                        },
                        _ => panic!("newarray called with non-int length"),
                    }
                },

                opcode::ARRAYLENGTH => {
                    let array_rc = pop_not_null!(Value::ArrayReference);
                    let len = array_rc.borrow().len();
                    push!(Value::Int(Wrapping(len)));
                },

                // TODO
                opcode::IFNULL => {
                    let branch_offset = self.read_next_short() as i16;
                    if let Value::NullReference = pop!() {
                        // 3 byte long instruction; read* operations move the PC.
                        let this_pc_start = self.pc - 3;
                        self.pc = (this_pc_start as i32 + branch_offset as i32) as u16
                    }
                },
                opcode::IFNONNULL => {
                    let branch_offset = self.read_next_short() as i16;
                    if let Value::NullReference = pop!() {
                        ()
                    } else {
                        // 3 byte long instruction; read* operations move the PC.
                        let this_pc_start = self.pc - 3;
                        self.pc = (this_pc_start as i32 + branch_offset as i32) as u16
                    }
                },

                // TODO

                _ => {
                    println!("{}", self.code[(self.pc as usize) - 1]);
                    panic!("unknown or reserved opcode")
                },
            }
        }
    }
}
