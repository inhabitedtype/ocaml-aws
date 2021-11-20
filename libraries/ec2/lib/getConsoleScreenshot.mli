open Types
type input = GetConsoleScreenshotRequest.t
type output = GetConsoleScreenshotResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error