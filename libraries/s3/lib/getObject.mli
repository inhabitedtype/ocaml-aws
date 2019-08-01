open Types
type input = GetObjectRequest.t
type output = GetObjectOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error