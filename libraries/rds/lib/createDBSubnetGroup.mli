open Types
type input = CreateDBSubnetGroupMessage.t
type output = CreateDBSubnetGroupResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)