open Types
type input = ModifyVpcPeeringConnectionOptionsRequest.t
type output = ModifyVpcPeeringConnectionOptionsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error