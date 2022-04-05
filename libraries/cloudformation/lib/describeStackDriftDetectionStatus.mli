open Types
type input = DescribeStackDriftDetectionStatusInput.t
type output = DescribeStackDriftDetectionStatusOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error