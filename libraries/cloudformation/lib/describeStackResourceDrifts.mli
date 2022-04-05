open Types
type input = DescribeStackResourceDriftsInput.t
type output = DescribeStackResourceDriftsOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error