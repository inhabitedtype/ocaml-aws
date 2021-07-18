open Types
type input = DescribeStreamProcessorRequest.t
type output = DescribeStreamProcessorResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error