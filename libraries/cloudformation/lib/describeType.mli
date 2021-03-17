open Types
type input = DescribeTypeInput.t
type output = DescribeTypeOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error