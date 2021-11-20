open Types
type input = DescribeTransitGatewayMulticastDomainsRequest.t
type output = DescribeTransitGatewayMulticastDomainsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error