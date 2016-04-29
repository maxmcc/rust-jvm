use vm::{Method, Value};
use vm::constant_pool::{constant_pool_index, RuntimeConstantPool, RuntimeConstantPoolEntry};

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

mod opcode {
    pub const NOP: u8 = 0x00;
    pub const ACONST_NULL: u8 = 0x01;
    pub const ICONST_M1: u8 = 0x02;
    pub const ICONST_0: u8 = 0x03;
    pub const ICONST_1: u8 = 0x04;
    pub const ICONST_2: u8 = 0x05;
    pub const ICONST_3: u8 = 0x06;
    pub const ICONST_4: u8 = 0x07;
    pub const ICONST_5: u8 = 0x08;
    pub const LCONST_0: u8 = 0x09;
    pub const LCONST_1: u8 = 0x0a;
    pub const FCONST_0: u8 = 0x0b;
    pub const FCONST_1: u8 = 0x0c;
    pub const FCONST_2: u8 = 0x0d;
    pub const DCONST_0: u8 = 0x0e;
    pub const DCONST_1: u8 = 0x0f;
    pub const BIPUSH: u8 = 0x10;
    pub const SIPUSH: u8 = 0x11;
    pub const LDC: u8 = 0x12;
    pub const LDC_W: u8 = 0x13;
    pub const LDC2_W: u8 = 0x14;
    pub const ILOAD: u8 = 0x15;
    pub const LLOAD: u8 = 0x16;
    pub const FLOAD: u8 = 0x17;
    pub const DLOAD: u8 = 0x18;
    pub const ALOAD: u8 = 0x19;
    pub const ILOAD_0: u8 = 0x1a;
    pub const ILOAD_1: u8 = 0x1b;
    pub const ILOAD_2: u8 = 0x1c;
    pub const ILOAD_3: u8 = 0x1d;
    pub const LLOAD_0: u8 = 0x1e;
    pub const LLOAD_1: u8 = 0x1f;
    pub const LLOAD_2: u8 = 0x20;
    pub const LLOAD_3: u8 = 0x21;
    pub const FLOAD_0: u8 = 0x22;
    pub const FLOAD_1: u8 = 0x23;
    pub const FLOAD_2: u8 = 0x24;
    pub const FLOAD_3: u8 = 0x25;
    pub const DLOAD_0: u8 = 0x26;
    pub const DLOAD_1: u8 = 0x27;
    pub const DLOAD_2: u8 = 0x28;
    pub const DLOAD_3: u8 = 0x29;
    pub const ALOAD_0: u8 = 0x2a;
    pub const ALOAD_1: u8 = 0x2b;
    pub const ALOAD_2: u8 = 0x2c;
    pub const ALOAD_3: u8 = 0x2d;
    pub const IALOAD: u8 = 0x2e;
    pub const LALOAD: u8 = 0x2f;
    pub const FALOAD: u8 = 0x30;
    pub const DALOAD: u8 = 0x31;
    pub const AALOAD: u8 = 0x32;
    pub const BALOAD: u8 = 0x33;
    pub const CALOAD: u8 = 0x34;
    pub const SALOAD: u8 = 0x35;
    pub const ISTORE: u8 = 0x36;
    pub const LSTORE: u8 = 0x37;
    pub const FSTORE: u8 = 0x38;
    pub const DSTORE: u8 = 0x39;
    pub const ASTORE: u8 = 0x3a;
    pub const ISTORE_0: u8 = 0x3b;
    pub const ISTORE_1: u8 = 0x3c;
    pub const ISTORE_2: u8 = 0x3d;
    pub const ISTORE_3: u8 = 0x3e;
    pub const LSTORE_0: u8 = 0x3f;
    pub const LSTORE_1: u8 = 0x40;
    pub const LSTORE_2: u8 = 0x41;
    pub const LSTORE_3: u8 = 0x42;
    pub const FSTORE_0: u8 = 0x43;
    pub const FSTORE_1: u8 = 0x44;
    pub const FSTORE_2: u8 = 0x45;
    pub const FSTORE_3: u8 = 0x46;
    pub const DSTORE_0: u8 = 0x47;
    pub const DSTORE_1: u8 = 0x48;
    pub const DSTORE_2: u8 = 0x49;
    pub const DSTORE_3: u8 = 0x4a;
    pub const ASTORE_0: u8 = 0x4b;
    pub const ASTORE_1: u8 = 0x4c;
    pub const ASTORE_2: u8 = 0x4d;
    pub const ASTORE_3: u8 = 0x4e;
    pub const IASTORE: u8 = 0x4f;
    pub const LASTORE: u8 = 0x50;
    pub const FASTORE: u8 = 0x51;
    pub const DASTORE: u8 = 0x52;
    pub const AASTORE: u8 = 0x53;
    pub const BASTORE: u8 = 0x54;
    pub const CASTORE: u8 = 0x55;
    pub const SASTORE: u8 = 0x56;
    pub const POP: u8 = 0x57;
    pub const POP2: u8 = 0x58;
    pub const DUP: u8 = 0x59;
    pub const DUP_X1: u8 = 0x5a;
    pub const DUP_X2: u8 = 0x5b;
    pub const DUP2: u8 = 0x5c;
    pub const DUP2_X1: u8 = 0x5d;
    pub const DUP2_X2: u8 = 0x5e;
    pub const SWAP: u8 = 0x5f;
    pub const IADD: u8 = 0x60;
    pub const LADD: u8 = 0x61;
    pub const FADD: u8 = 0x62;
    pub const DADD: u8 = 0x63;
    pub const ISUB: u8 = 0x64;
    pub const LSUB: u8 = 0x65;
    pub const FSUB: u8 = 0x66;
    pub const DSUB: u8 = 0x67;
    pub const IMUL: u8 = 0x68;
    pub const LMUL: u8 = 0x69;
    pub const FMUL: u8 = 0x6a;
    pub const DMUL: u8 = 0x6b;
    pub const IDIV: u8 = 0x6c;
    pub const LDIV: u8 = 0x6d;
    pub const FDIV: u8 = 0x6e;
    pub const DDIV: u8 = 0x6f;
    pub const IREM: u8 = 0x70;
    pub const LREM: u8 = 0x71;
    pub const FREM: u8 = 0x72;
    pub const DREM: u8 = 0x73;
    pub const INEG: u8 = 0x74;
    pub const LNEG: u8 = 0x75;
    pub const FNEG: u8 = 0x76;
    pub const DNEG: u8 = 0x77;
    pub const ISHL: u8 = 0x78;
    pub const LSHL: u8 = 0x79;
    pub const ISHR: u8 = 0x7a;
    pub const LSHR: u8 = 0x7b;
    pub const IUSHR: u8 = 0x7c;
    pub const LUSHR: u8 = 0x7d;
    pub const IAND: u8 = 0x7e;
    pub const LAND: u8 = 0x7f;
    pub const IOR: u8 = 0x80;
    pub const LOR: u8 = 0x81;
    pub const IXOR: u8 = 0x82;
    pub const LXOR: u8 = 0x83;
    pub const IINC: u8 = 0x84;
    pub const I2L: u8 = 0x85;
    pub const I2F: u8 = 0x86;
    pub const I2D: u8 = 0x87;
    pub const L2I: u8 = 0x88;
    pub const L2F: u8 = 0x89;
    pub const L2D: u8 = 0x8a;
    pub const F2I: u8 = 0x8b;
    pub const F2L: u8 = 0x8c;
    pub const F2D: u8 = 0x8d;
    pub const D2I: u8 = 0x8e;
    pub const D2L: u8 = 0x8f;
    pub const D2F: u8 = 0x90;
    pub const I2B: u8 = 0x91;
    pub const I2C: u8 = 0x92;
    pub const I2S: u8 = 0x93;
    pub const LCMP: u8 = 0x94;
    pub const FCMPL: u8 = 0x95;
    pub const FCMPG: u8 = 0x96;
    pub const DCMPL: u8 = 0x97;
    pub const DCMPG: u8 = 0x98;
    pub const IFEQ: u8 = 0x99;
    pub const IFNE: u8 = 0x9a;
    pub const IFLT: u8 = 0x9b;
    pub const IFGE: u8 = 0x9c;
    pub const IFGT: u8 = 0x9d;
    pub const IFLE: u8 = 0x9e;
    pub const IF_ICMPEQ: u8 = 0x9f;
    pub const IF_ICMPNE: u8 = 0xa0;
    pub const IF_ICMPLT: u8 = 0xa1;
    pub const IF_ICMPGE: u8 = 0xa2;
    pub const IF_ICMPGT: u8 = 0xa3;
    pub const IF_ICMPLE: u8 = 0xa4;
    pub const IF_ACMPEQ: u8 = 0xa5;
    pub const IF_ACMPNE: u8 = 0xa6;
    pub const GOTO: u8 = 0xa7;
    pub const JSR: u8 = 0xa8;
    pub const RET: u8 = 0xa9;
    pub const TABLESWITCH: u8 = 0xaa;
    pub const LOOKUPSWITCH: u8 = 0xab;
    pub const IRETURN: u8 = 0xac;
    pub const LRETURN: u8 = 0xad;
    pub const FRETURN: u8 = 0xae;
    pub const DRETURN: u8 = 0xaf;
    pub const ARETURN: u8 = 0xb0;
    pub const RETURN: u8 = 0xb1;
    pub const GETSTATIC: u8 = 0xb2;
    pub const PUTSTATIC: u8 = 0xb3;
    pub const GETFIELD: u8 = 0xb4;
    pub const PUTFIELD: u8 = 0xb5;
    pub const INVOKEVIRTUAL: u8 = 0xb6;
    pub const INVOKESPECIAL: u8 = 0xb7;
    pub const INVOKESTATIC: u8 = 0xb8;
    pub const INVOKEINTERFACE: u8 = 0xb9;
    pub const INVOKEDYNAMIC: u8 = 0xba;
    pub const NEW: u8 = 0xbb;
    pub const NEWARRAY: u8 = 0xbc;
    pub const ANEWARRAY: u8 = 0xbd;
    pub const ARRAYLENGTH: u8 = 0xbe;
    pub const ATHROW: u8 = 0xbf;
    pub const CHECKCAST: u8 = 0xc0;
    pub const INSTANCEOF: u8 = 0xc1;
    pub const MONITORENTER: u8 = 0xc2;
    pub const MONITOREXIT: u8 = 0xc3;
    pub const WIDE: u8 = 0xc4;
    pub const MULTIANEWARRAY: u8 = 0xc5;
    pub const IFNULL: u8 = 0xc6;
    pub const IFNONNULL: u8 = 0xc7;
    pub const GOTO_W: u8 = 0xc8;
    pub const JSR_W: u8 = 0xc9;
    pub const BREAKPOINT: u8 = 0xca;
    pub const IMPDEP1: u8 = 0xfe;
    pub const IMPDEP2: u8 = 0xff;
}
