open Types
type input = DescribeExportTasksMessage.t
type output = ExportTasksMessage.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error