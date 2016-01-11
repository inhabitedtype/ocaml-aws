open Types
type input = DescribeAccessPointsInput.t
type output = DescribeAccessPointsOutput.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)