open Types
type input = DeleteReplicationGroupMessage.t
type output = DeleteReplicationGroupResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)