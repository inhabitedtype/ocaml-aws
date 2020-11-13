open Types
type input = CreateVpcPeeringConnectionRequest.t
type output = CreateVpcPeeringConnectionResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error