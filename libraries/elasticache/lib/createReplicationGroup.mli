open Types
type input = CreateReplicationGroupMessage.t
type output = CreateReplicationGroupResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)