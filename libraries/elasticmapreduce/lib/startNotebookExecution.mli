open Types
type input = StartNotebookExecutionInput.t
type output = StartNotebookExecutionOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error