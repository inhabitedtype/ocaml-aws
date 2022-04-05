open Types
type input = ModifyVpcEndpointServiceConfigurationRequest.t
type output = ModifyVpcEndpointServiceConfigurationResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error