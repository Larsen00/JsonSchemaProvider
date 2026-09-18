namespace JsonSchemaProvider.DesignTime

module ProviderConfiguration =
    open System
    open FSharp.Quotations

    type CompileFlags = {
        CompileMinItems: bool
    }

    // Everything there is to know about turning one JSON-schema-derived node into F#: its
    // compile-time type (what property/parameter signatures show - can differ from RuntimeType
    // for erased types like classes), its runtime/erased type, and the two conversion functions
    // between JsonValue and that runtime type. Built together, in one recursive pass per node
    // kind, so a node's type shape and its conversion logic can never drift out of sync with
    // each other the way they could when they lived in separate functions.
    // NOTE: This use to be 4 diffent recursive functions but after adding compile time support 
    // for specific keywords it became to complex to ensure all 4 functions stayed in sync.
    type NodeConversion = {
        CompileTimeType: Type
        RuntimeType: Type
        ToRuntime: Expr // closed lambda: JsonValue -> RuntimeType
        ToJson: Expr // closed lambda: RuntimeType -> JsonValue
        FullyCompilable: bool // Determines if the node conversion can be fully compiled and therefore skip validation at runtime
    }