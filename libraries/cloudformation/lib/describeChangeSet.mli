open Types
type input = DescribeChangeSetInput.t
type output = DescribeChangeSetOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error