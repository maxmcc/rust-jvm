use vm::value::Value;

type NativeMethod = FnOnce(Vec<Value>) -> Option<Value>;




