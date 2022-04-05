open Types
type input = ExportImageRequest.t
type output = ExportImageResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error