use model::class_file::access_flags::class_access_flags;

use vm::{Class, Method, MethodCode, Value};
use vm::class_loader::ClassLoader;
use vm::constant_pool::{RuntimeConstantPool, RuntimeConstantPoolEntry};
use vm::bytecode::opcode;

/// A frame is used to store data and partial results, as well as to perform dynamic linking,
/// return values for methods, and dispatch exceptions.
#[derive(Debug)]
pub struct Frame<'a> {
    /// A reference to the class containing the currently executing method.
    current_class: &'a Class,
    /// Contains the bytecode currently executing in this frame, along with related structures.
    method_code: &'a MethodCode,
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
    pub fn new(current_class: &'a Class, method_code: &'a MethodCode,
               local_variables: Vec<Option<Value>>) -> Self {
        Frame {
            current_class: current_class,
            method_code: method_code,
            pc: 0,
            local_variables: local_variables,
            operand_stack: vec![],
        }
    }

    fn read_next_byte(&mut self) -> u8 {
        let result = self.method_code.code[self.pc as usize];
        self.pc += 1;
        result
    }

    fn read_next_short(&mut self) -> u16 {
        ((self.read_next_byte() as u16) << 8) | (self.read_next_byte() as u16)
    }

    fn pop_as_locals(&mut self, count: usize) -> Vec<Option<Value>> {
        let mut result = vec![];
        let start_index = self.operand_stack.len() - count - 1;
        for value in self.operand_stack.split_at(start_index).1 {
            result.push(Some(value.clone()));
            match *value {
                Value::Long(_) | Value::Double(_) => result.push(None),
                _ => (),
            }
        }
        result
    }

    fn invoke(&mut self, class_loader: &mut ClassLoader, class: &Class, method: &Method,
              args: Vec<Option<Value>>) {
        match method.method_code {
            None => (),
            Some(ref method_code) => {
                let frame = Frame::new(class, method_code, args);
                match frame.run(class_loader) {
                    None => (),
                    Some(return_value) => self.operand_stack.push(return_value)
                }
            }
        }
    }

    pub fn run(mut self, class_loader: &mut ClassLoader) -> Option<Value> {
        macro_rules! with {
            ($read_next_action: ident, $k: ident) => ({
                let value = self.$read_next_action() as u16;
                $k!(value);
            })
        }

        macro_rules! do_ipush {
            ($value: ident) => (self.operand_stack.push(Value::Int($value as i32)))
        }

        macro_rules! do_ldc {
            ($index: ident) => ({
                match self.current_class.get_constant_pool()[$index] {
                    Some(RuntimeConstantPoolEntry::PrimitiveLiteral(ref value)) =>
                        self.operand_stack.push(value.clone()),
                    _ => panic!("illegal or unsupported constant pool load"),
                }
            });
        }

        loop {
            match self.read_next_byte() {
                opcode::NOP => (),
                opcode::ACONST_NULL => self.operand_stack.push(Value::NullReference),
                opcode::ICONST_M1 => self.operand_stack.push(Value::Int(-1)),
                opcode::ICONST_0 => self.operand_stack.push(Value::Int(0)),
                opcode::ICONST_1 => self.operand_stack.push(Value::Int(1)),
                opcode::ICONST_2 => self.operand_stack.push(Value::Int(2)),
                opcode::ICONST_3 => self.operand_stack.push(Value::Int(3)),
                opcode::ICONST_4 => self.operand_stack.push(Value::Int(4)),
                opcode::ICONST_5 => self.operand_stack.push(Value::Int(5)),
                opcode::LCONST_0 => self.operand_stack.push(Value::Long(0)),
                opcode::LCONST_1 => self.operand_stack.push(Value::Long(1)),
                opcode::FCONST_0 => self.operand_stack.push(Value::Float(0.0)),
                opcode::FCONST_1 => self.operand_stack.push(Value::Float(1.0)),
                opcode::FCONST_2 => self.operand_stack.push(Value::Float(2.0)),
                opcode::DCONST_0 => self.operand_stack.push(Value::Double(0.0)),
                opcode::DCONST_1 => self.operand_stack.push(Value::Double(1.0)),
                opcode::BIPUSH => with!(read_next_byte, do_ipush),
                opcode::SIPUSH => with!(read_next_short, do_ipush),
                opcode::LDC => with!(read_next_byte, do_ldc),
                opcode::LDC_W | opcode::LDC2_W => with!(read_next_short, do_ldc),

                opcode::POP => {
                    self.operand_stack.pop();
                },
                opcode::POP2 => {
                    match self.operand_stack.pop() {
                        Some(Value::Long(_)) | Some(Value::Double(_)) => (),
                        _ => {
                            self.operand_stack.pop();
                        },
                    }
                },
                opcode::DUP => {
                    let value = self.operand_stack.last().unwrap().clone();
                    self.operand_stack.push(value);
                },
                opcode::DUP_X1 => {
                    let value1 = self.operand_stack.pop().unwrap();
                    let value2 = self.operand_stack.pop().unwrap();
                    self.operand_stack.extend_from_slice(&[value1.clone(), value2, value1]);
                },
                opcode::DUP_X2 => {
                    let value1 = self.operand_stack.pop().unwrap();
                    let value2 = self.operand_stack.pop().unwrap();
                    match value2 {
                        Value::Long(_) | Value::Double(_) => {
                            self.operand_stack.extend_from_slice(&[value1.clone(), value2, value1]);
                        },
                        _ => {
                            let value3 = self.operand_stack.pop().unwrap();
                            self.operand_stack.extend_from_slice(
                                &[value1.clone(), value3, value2, value1]);
                        },
                    }
                },
                opcode::DUP2 => {
                    let value1 = self.operand_stack.pop().unwrap();
                    match value1 {
                        Value::Long(_) | Value::Double(_) => {
                            self.operand_stack.extend_from_slice(&[value1.clone(), value1]);
                        },
                        _ => {
                            let value2 = self.operand_stack.pop().unwrap();
                            self.operand_stack.extend_from_slice(
                                &[value2.clone(), value1.clone(), value2, value1]);
                        },
                    }
                },

                opcode::RETURN => return None,

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
                        let args = self.pop_as_locals(num_args + 1);
                        let object_class = {
                            let object_value = &args[0];
                            if let Some(Value::NullReference) = *object_value {
                                panic!("NullPointerException")
                            } else if let Some(Value::Reference(ref object_ref)) = *object_value {
                                let object = object_ref.as_ref().borrow();
                                object.get_class().clone()
                            } else {
                                panic!("invokevirtual on a primitive type");
                            }
                        };
                        match object_class.dispatch_method(resolved_method) {
                            None => panic!("AbstractMethodError"),
                            Some((actual_class, actual_method)) =>
                                self.invoke(class_loader, actual_class, actual_method, args),
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
                        let args = self.pop_as_locals(num_args + 1);

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
                        self.invoke(class_loader, actual_class.as_ref(), actual_method, args)
                    } else {
                        panic!("invokespecial refers to non-method in constant pool");
                    }
                },

                _ => {
                    println!("{}", self.method_code.code[(self.pc as usize) - 1]);
                    panic!("undefined or reserved opcode")
                },
            }
        }
    }
}
