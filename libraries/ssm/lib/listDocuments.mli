open Types
type input = ListDocumentsRequest.t
type output = ListDocumentsResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)