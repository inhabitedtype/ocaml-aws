open Types
type input = DecreaseReplicaCountMessage.t
type output = DecreaseReplicaCountResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error