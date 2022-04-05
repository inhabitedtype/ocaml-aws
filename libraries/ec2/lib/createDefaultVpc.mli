open Types
type input = CreateDefaultVpcRequest.t
type output = CreateDefaultVpcResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error