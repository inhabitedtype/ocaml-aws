open Types
type input = DescribeNetworkInterfacesRequest.t
type output = DescribeNetworkInterfacesResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)