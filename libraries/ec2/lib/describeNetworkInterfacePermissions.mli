open Types
type input = DescribeNetworkInterfacePermissionsRequest.t
type output = DescribeNetworkInterfacePermissionsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error