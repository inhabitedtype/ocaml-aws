open Types
type input = CreateVpcEndpointServiceConfigurationRequest.t
type output = CreateVpcEndpointServiceConfigurationResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error