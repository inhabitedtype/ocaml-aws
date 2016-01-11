open Types
type input = DescribeStackResourceInput.t
type output = DescribeStackResourceOutput.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)