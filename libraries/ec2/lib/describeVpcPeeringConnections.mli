open Types
type input = DescribeVpcPeeringConnectionsRequest.t
type output = DescribeVpcPeeringConnectionsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error