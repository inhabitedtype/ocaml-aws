open Types
type input = DescribeNotebookExecutionInput.t
type output = DescribeNotebookExecutionOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error