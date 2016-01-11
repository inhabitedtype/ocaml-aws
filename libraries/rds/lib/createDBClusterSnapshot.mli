open Types
type input = CreateDBClusterSnapshotMessage.t
type output = CreateDBClusterSnapshotResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)