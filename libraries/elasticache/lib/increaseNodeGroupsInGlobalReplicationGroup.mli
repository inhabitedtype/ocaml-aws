open Types
type input = IncreaseNodeGroupsInGlobalReplicationGroupMessage.t
type output = IncreaseNodeGroupsInGlobalReplicationGroupResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error