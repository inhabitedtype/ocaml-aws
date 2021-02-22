open Types
type input = DescribeTableInput.t
type output = DescribeTableOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error