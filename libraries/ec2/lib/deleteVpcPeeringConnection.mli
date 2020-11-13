open Types
type input = DeleteVpcPeeringConnectionRequest.t
type output = DeleteVpcPeeringConnectionResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error