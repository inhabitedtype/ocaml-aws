open Types
type input = DescribeAccountAttributesRequest.t
type output = DescribeAccountAttributesResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)