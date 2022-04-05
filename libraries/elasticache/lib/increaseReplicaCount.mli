open Types
type input = IncreaseReplicaCountMessage.t
type output = IncreaseReplicaCountResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error