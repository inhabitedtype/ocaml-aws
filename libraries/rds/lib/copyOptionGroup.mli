open Types
type input = CopyOptionGroupMessage.t
type output = CopyOptionGroupResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error