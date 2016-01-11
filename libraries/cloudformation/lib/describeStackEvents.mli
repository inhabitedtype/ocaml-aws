open Types
type input = DescribeStackEventsInput.t
type output = DescribeStackEventsOutput.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)