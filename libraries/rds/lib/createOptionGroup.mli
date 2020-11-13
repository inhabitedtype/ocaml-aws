open Types
type input = CreateOptionGroupMessage.t
type output = CreateOptionGroupResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error