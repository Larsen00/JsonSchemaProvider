namespace JsonSchemaProvider.DesignTime

// Shared types for the design-time code: node conversion results and per-run configuration.
module ProviderConfiguration =
    open System
    open System.Reflection
    open System.Collections.Concurrent
    open FSharp.Quotations

    // Types and conversions for one schema node, built together so they stay in sync.
    type NodeConversion = {
        CompileTimeType: Type // shown in signatures; differs from RuntimeType for erased types
        RuntimeType: Type
        ToRuntime: Expr // closed lambda: JsonValue -> RuntimeType
        ToJson: Expr // closed lambda: RuntimeType -> JsonValue
        FullyCompilable: bool // true means runtime validation can be skipped
    }

    // A ToList/ToTuple helper and the return type of its provided method.
    type RuntimeHelperConversion = {
        Convert: Expr // closed lambda on runtime types
        CompileTimeReturnType: Type
    }

    // Static helpers exposed on "<Property>Array" / "ItemArray" types.
    type ArrayHelpers = {
        ToList: RuntimeHelperConversion // any array shape -> 'T list
        ToTuple: RuntimeHelperConversion option // only ExactLength with n >= 2 -> flat n-tuple
    }

    // An array's node conversion plus its array-only helpers.
    type ArrayConversion = {
        common: NodeConversion
        specific: ArrayHelpers
    }

    // User-set flags from the provider's static parameters.
    type CompileFlags = {
        SkipRuntimeValidation: bool // unsafe: invalid JSON may fail later at runtime
        IgnoreSpecificKeywords: bool // fall back to runtime validation for these keywords
    }

    // Shared data for one generation run, passed through every generation function.
    type GenerationContext = {
        Assembly: Assembly
        NamespaceName: string
        RootBaseType: Type
        SchemaHashCode: int32
        SchemaString: string
        // #omit-start
        CompileFlags: CompileFlags
        // #omit-end
        ConversionCache: ConcurrentDictionary<string, NodeConversion> // keyed by node Path
    }
