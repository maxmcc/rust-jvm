/// An bytecode instruction in the Java virtual machine.
#[derive(Debug)]
pub enum Instruction {
    Constant(op::Constant),
    Load(op::Load),
    Store(op::Store),
    Stack(op::Stack),
    Math(op::Math),
    Conversion(op::Conversion),
    Comparison(op::Comparison),
    Control(op::Control),
    Reference(op::Reference),
    Extended(op::Extended),
}

mod op {
    use model::class_file::constant_pool_index;

    pub type LocalVariableIndex = u16;
    pub type BranchOffset = i32;

    /// Push a constant value onto the operand stack.
    #[derive(Debug)]
    #[allow(non_camel_case_types)]
    pub enum Constant {
        Aconst_null,
        Ipush { value: i32 },
        Lconst_0,
        Lconst_1,
        Fconst_0,
        Fconst_1,
        Fconst_2,
        Dconst_0,
        Dconst_1,
        Ldc { index: constant_pool_index },
    }

    /// Load a value from a local variable or array onto the operand stack.
    #[derive(Debug)]
    #[allow(non_camel_case_types)]
    pub enum Load {
        Iload { index: LocalVariableIndex },
        Lload { index: LocalVariableIndex },
        Fload { index: LocalVariableIndex },
        Dload { index: LocalVariableIndex },
        Aload { index: LocalVariableIndex },
        Iaload,
        Laload,
        Faload,
        Daload,
        Aaload,
        Baload,
        Caload,
        Saload,
    }

    /// Store a value on the operand stack into a local variable or array.
    #[derive(Debug)]
    #[allow(non_camel_case_types)]
    pub enum Store {
        Istore { index: LocalVariableIndex },
        Lstore { index: LocalVariableIndex },
        Fstore { index: LocalVariableIndex },
        Dstore { index: LocalVariableIndex },
        Astore { index: LocalVariableIndex },
        Iastore,
        Lastore,
        Fastore,
        Dastore,
        Aastore,
        Bastore,
        Castore,
        Sastore,
    }

    /// Manipulate values on the operand stack.
    #[derive(Debug)]
    #[allow(non_camel_case_types)]
    pub enum Stack {
        Pop,
        Pop2,
        Dup,
        Dup_x1,
        Dup_x2,
        Dup2,
        Dup2_x1,
        Dup2_x2,
        Swap,
    }

    /// Perform mathematical operations on stack values.
    #[derive(Debug)]
    #[allow(non_camel_case_types)]
    pub enum Math {
        Add,
        Sub,
        Mul,
        Div,
        Rem,
        Neg,
        Shl,
        Shr,
        Ushr,
        And,
        Or,
        Xor,
        Iinc { index: LocalVariableIndex, constant: i16 },
    }

    /// Convert stack values between JVM types.
    #[derive(Debug)]
    pub enum Conversion {
        I2l,
        I2f,
        I2d,
        L2i,
        L2f,
        L2d,
        F2i,
        F2l,
        F2d,
        D2i,
        D2l,
        D2f,
        I2b,
        I2c,
        I2s,
    }

    /// Compare stack values conditionally alter control flow.
    #[derive(Debug)]
    #[allow(non_camel_case_types)]
    pub enum Comparison {
        Lcmp,
        Fcmpl,
        Fcmpg,
        Dcmpl,
        Dcmpg,
        Ifeq { offset: BranchOffset },
        Ifne { offset: BranchOffset },
        Iflt { offset: BranchOffset },
        Ifge { offset: BranchOffset },
        Ifgt { offset: BranchOffset },
        Ifle { offset: BranchOffset },
        If_icmpeq { offset: BranchOffset },
        If_icmpne { offset: BranchOffset },
        If_icmplt { offset: BranchOffset },
        If_icmpge { offset: BranchOffset },
        If_icmpgt { offset: BranchOffset },
        If_icmple { offset: BranchOffset },
        If_acmpeq { offset: BranchOffset },
        If_acmpne { offset: BranchOffset },
    }

    /// Unconditionally modify control flow.
    #[derive(Debug)]
    pub enum Control {
        Goto { offset: BranchOffset },
        Tableswitch { bytes: Vec<u8> },
        Lookupswitch { bytes: Vec<u8> },
        ReturnValue,
        ReturnVoid,
    }

    /// Operations on reference types (objects and arrays).
    #[derive(Debug)]
    pub enum Reference {
        Getstatic { index: constant_pool_index },
        Putstatic { index: constant_pool_index },
        Getfield { index: constant_pool_index },
        Putfield { index: constant_pool_index },
        Invokevirtual { index: constant_pool_index },
        Invokespecial { index: constant_pool_index },
        Invokestatic { index: constant_pool_index },
        New { index: constant_pool_index },
        Newarray { element_type: u8 },
        Anewarray { index: constant_pool_index },
        Arraylength,
        Athrow,
        Checkcast { index: constant_pool_index },
        Instanceof { index: constant_pool_index },
        Monitorenter,
        Monitorexit,
    }

    /// Extended instructions supported by the JVM.
    #[derive(Debug)]
    #[allow(non_camel_case_types)]
    pub enum Extended {
        Multianewarray { index: constant_pool_index, dimensions: u8 },
        Ifnull { offset: BranchOffset },
        Ifnonnull { offset: BranchOffset },
        Goto_w,
    }
}

