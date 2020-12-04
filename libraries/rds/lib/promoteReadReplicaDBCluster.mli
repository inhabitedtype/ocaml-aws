open Types
type input = PromoteReadReplicaDBClusterMessage.t
type output = PromoteReadReplicaDBClusterResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error