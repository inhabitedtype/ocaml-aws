open Types
type input = CancelImportTaskRequest.t
type output = CancelImportTaskResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error