open Types
type input = ModifyVpcEndpointServicePermissionsRequest.t
type output = ModifyVpcEndpointServicePermissionsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error