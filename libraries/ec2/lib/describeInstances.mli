open Types
type input = DescribeInstancesRequest.t
type output = DescribeInstancesResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)