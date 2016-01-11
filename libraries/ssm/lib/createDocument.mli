open Types
type input = CreateDocumentRequest.t
type output = CreateDocumentResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)