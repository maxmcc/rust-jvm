use vm::Value;
use vm::constant_pool::RuntimeConstantPool;

/// A frame is used to store data and partial results, as well as to perform dynamic linking,
/// return values for methods, and dispatch exceptions.
#[derive(Debug)]
pub struct Frame<'a> {
    /// The local variables of the current method.
    /// Values that occupy two indices (`long` and `double`) are stored in one slot followed by a
    /// `None` value in the subsequent index.
    local_variables: Vec<Option<Value>>,
    /// The operand stack manipulated by the instructions of the current method.
    operand_stack: Vec<Value>,
    /// A reference to the runtime constant pool of the current method's class.
    runtime_constant_pool: &'a RuntimeConstantPool,
}

