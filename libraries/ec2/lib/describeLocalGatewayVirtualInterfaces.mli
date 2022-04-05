open Types
type input = DescribeLocalGatewayVirtualInterfacesRequest.t
type output = DescribeLocalGatewayVirtualInterfacesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error