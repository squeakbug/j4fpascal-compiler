
/// Pascal in predecessor of C#, so let's take .NET type system
/// https://learn.microsoft.com/en-us/dotnet/csharp/fundamentals/types/

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjectType {
    id: String,
    ident: String,
}

#[derive(Debug, Clone)]
pub struct InterfaceType {
    base: ObjectType,
}

#[derive(Debug, Clone)]
pub struct ValueType {
    base: ObjectType,
    kind: ValueTypeKind,
}

/// Result of tree-walking
#[derive(Debug, Clone)]
pub enum ValueTypeKind {
    Boolean(bool),
    Char(char),
    Byte(u8),
    SByte(i8),
    UShort(u16),
    Short(i16),
    UInteger(u32),
    Integer(i32),
    ULong(u64),
    Long(i64),
    Float(f32),
    Double(f64),
    String(String),
    Record {
        fields: Vec<(String, )>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringType {   
    parents: Vec<Box<Type>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {   
    parents: Vec<Box<Type>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionalType {
    Procedure(ProcedureValue),
    // TODO: When classes arive, change to some reference type
    Ref(Designator),
    Record(HashMap<String, Value>),
}

#[derive(Debug, Clone)]
pub struct Symbol {
    sym_type: Type,
    value: Option<Value>,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::UnsignedInteger(num) => write!(f, "{:?}", num),
            Value::UnsignedReal(num) => write!(f, "{:?}", num),
            Value::String(s) => write!(f, "\"{:?}\"", s),
            Value::Boolean(b) => write!(f, "{:?}", b),
            Value::Procedure(p) => write!(f, "{:?}", p),
            Value::Ref(d) => write!(f, "{:?}", d),
            Value::Record(r) => write!(f, "{:?}", r),
            Value::Null => write!(f, "null"),
        }
    }
}