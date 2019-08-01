open Types
type input = CreateVpcRequest.t
type output = CreateVpcResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error