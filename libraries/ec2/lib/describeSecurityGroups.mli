open Types
type input = DescribeSecurityGroupsRequest.t
type output = DescribeSecurityGroupsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error