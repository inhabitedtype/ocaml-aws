open Types
type input = ModifyReplicationGroupMessage.t
type output = ModifyReplicationGroupResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error