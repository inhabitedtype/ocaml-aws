open Types
type input = CreateDBInstanceReadReplicaMessage.t
type output = CreateDBInstanceReadReplicaResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error