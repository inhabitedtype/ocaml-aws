open Types
type input = DeleteVpcEndpointsRequest.t
type output = DeleteVpcEndpointsResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)