open Types
type input = DescribeEndPointStateInput.t
type output = DescribeEndPointStateOutput.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)