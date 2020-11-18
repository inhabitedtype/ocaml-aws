open Types
type input = DescribeVpcEndpointServicesRequest.t
type output = DescribeVpcEndpointServicesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error