use vm::{Method, Value};
use vm::constant_pool::{constant_pool_index, RuntimeConstantPool, RuntimeConstantPoolEntry};
use vm::bytecode::opcode;

/// A frame is used to store data and partial results, as well as to perform dynamic linking,
/// return values for methods, and dispatch exceptions.
#[derive(Debug)]
pub struct Frame<'a> {
    /// The method executing in this frame. This structure contains the running bytecode.
    method: &'a Method,
    /// A reference to the runtime constant pool of the current method's class.
    runtime_constant_pool: &'a RuntimeConstantPool,
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
    pub fn new(method: &'a Method, runtime_constant_pool: &'a RuntimeConstantPool,
               local_variables: Vec<Option<Value>>) -> Self {
        Frame {
            method: method,
            runtime_constant_pool: runtime_constant_pool,
            pc: 0,
            local_variables: local_variables,
            operand_stack: vec![],
        }
    }

    fn read_next_byte(&mut self) -> u8 {
        let result = self.method.code[self.pc as usize];
        self.pc += 1;
        result
    }

    fn read_next_short(&mut self) -> u16 {
        ((self.read_next_byte() as u16) << 8) | (self.read_next_byte() as u16)
    }

    pub fn run(mut self) -> Option<Value> {
        macro_rules! with_index {
            ($read_next_action: ident, $with_i: ident) => ({
                let i = self.$read_next_action() as constant_pool_index;
                $with_i!(i);
            })
        }

        macro_rules! ldc_action {
            ($index: ident) => ({
                match self.runtime_constant_pool[$index] {
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
                opcode::BIPUSH => {
                    let value = Value::Int(self.read_next_byte() as i32);
                    self.operand_stack.push(value)
                },
                opcode::SIPUSH => {
                    let value = Value::Int(self.read_next_short() as i32);
                    self.operand_stack.push(value)
                },
                opcode::LDC => with_index!(read_next_byte, ldc_action),
                opcode::LDC_W | opcode::LDC2_W => with_index!(read_next_short, ldc_action),


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

                _ => panic!("undefined or reserved opcode"),
            }
        }
    }
}
