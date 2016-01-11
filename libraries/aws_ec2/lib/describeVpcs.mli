open Types
type input = DescribeVpcsRequest.t
type output = DescribeVpcsResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)