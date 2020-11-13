open Types
type input = ModifyOptionGroupMessage.t
type output = ModifyOptionGroupResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error