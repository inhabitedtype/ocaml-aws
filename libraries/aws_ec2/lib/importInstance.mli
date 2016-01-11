open Types
type input = ImportInstanceRequest.t
type output = ImportInstanceResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)