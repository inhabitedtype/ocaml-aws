open Types
type input = UpdateAssociationStatusRequest.t
type output = UpdateAssociationStatusResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)