open Types
type input = DeleteObjectRequest.t
type output = DeleteObjectOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error