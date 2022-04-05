open Types
type input = DescribeVpcAttributeRequest.t
type output = DescribeVpcAttributeResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error