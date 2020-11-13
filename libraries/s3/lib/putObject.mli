open Types
type input = PutObjectRequest.t
type output = PutObjectOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error