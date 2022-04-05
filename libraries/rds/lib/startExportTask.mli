open Types
type input = StartExportTaskMessage.t
type output = ExportTask.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error