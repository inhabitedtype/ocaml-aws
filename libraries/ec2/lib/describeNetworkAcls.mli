open Types
type input = DescribeNetworkAclsRequest.t
type output = DescribeNetworkAclsResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)