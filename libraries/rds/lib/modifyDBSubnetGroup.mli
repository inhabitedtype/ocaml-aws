open Types
type input = ModifyDBSubnetGroupMessage.t
type output = ModifyDBSubnetGroupResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)