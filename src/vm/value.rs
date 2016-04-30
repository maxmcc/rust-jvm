//! Internal JVM representations of Java values.

use std::cell::RefCell;
use std::collections::HashMap;
use std::num::Wrapping;
use std::rc::Rc;

use vm::class::Class;
use vm::sig;

/// A value in the Java virtual machine.
#[derive(Debug, Clone)]
pub enum Value {
    /// A 32-bit signed integral type, representing the Java types `byte`, `char`, `short`, `int`,
    /// and `boolean`.
    Int(Wrapping<i32>),
    /// A 32-bit floating-point type, representing the Java type `float`.
    Float(f32),
    /// A 64-bit signed integral type, representing the Java type `long`.
    Long(Wrapping<i64>),
    /// A 64-bit floating-point type, representing the Java type `double`.
    Double(f64),
    /// A reference to a scalar Java object in the heap.
    ScalarReference(Rc<RefCell<Scalar>>),
    /// A reference to a Java array in the heap.
    ArrayReference(Rc<RefCell<Array>>),
    /// A reference to a Java object which is `null`.
    NullReference,
}

#[derive(Debug)]
/// An instance of a non-array object.
pub struct Scalar {
    /// A reference to the object's creating class.
    class: Rc<Class>,
    /// The instance (non-`static`) fields of the object.
    fields: HashMap<sig::Field, Value>,
}

// TODO the semantics of getting and putting fields are incorrect w/r/t inheritance
impl Scalar {
    pub fn new(class: Rc<Class>) -> Self {
        match class.symref.sig {
            sig::Class::Scalar(_) => {
                let field_sigs = class.collect_instance_fields();
                let mut fields = HashMap::new();
                for sig in field_sigs {
                    let value = sig.ty.default_value();
                    fields.insert(sig, value);
                }
                Scalar {
                    class: class,
                    fields: fields,
                }
            },
            sig::Class::Array(_) => panic!("can't construct scalar from array class"),
        }
    }

    pub fn get_class(&self) -> Rc<Class> {
        self.class.clone()
    }

    pub fn get_field(&self, sig: &sig::Field) -> Value {
        self.fields.get(sig).unwrap().clone()
    }

    pub fn put_field(&mut self, sig: sig::Field, value: Value) {
        self.fields.insert(sig, value);
    }
}

#[derive(Debug)]
/// An instance of an array object.
pub struct Array {
    /// A reference to the (synthetic) array class.
    class: Rc<Class>,
    /// The array data.
    array: Vec<Value>,
}

impl Array {
    pub fn new(class: Rc<Class>, length: i32) -> Self {
        if length < 0 {
            panic!("NegativeArraySizeException");
        }
        match class.symref.sig {
            sig::Class::Scalar(_) => panic!("can't construct array from scalar class"),
            sig::Class::Array(ref component_ty) => {
                let mut array = Vec::with_capacity(length as usize);
                for _ in 0..length {
                    array.push(component_ty.default_value());
                }
                Array {
                    class: class.clone(),
                    array: array,
                }
            },
        }
    }

    pub fn get_class(&self) -> Rc<Class> {
        self.class.clone()
    }

    pub fn get(&self, index: i32) -> Value {
        if index < 0 || (index as usize) >= self.array.len() {
            panic!("ArrayIndexOutOfBoundsException")
        }
        self.array[index as usize].clone()
    }

    pub fn put(&mut self, index: i32, value: Value) {
        if index < 0 || (index as usize) >= self.array.len() {
            panic!("ArrayIndexOutOfBoundsException");
        }
        self.array[index as usize] = value;
    }

    pub fn len(&self) -> i32 {
        self.array.len() as i32
    }
}

