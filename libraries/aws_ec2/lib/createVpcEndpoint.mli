open Types
type input = CreateVpcEndpointRequest.t
type output = CreateVpcEndpointResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)