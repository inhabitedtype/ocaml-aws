open Types
type input = CreateAssociationBatchRequest.t
type output = CreateAssociationBatchResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)