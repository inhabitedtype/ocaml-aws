open Types
type input = DeleteDBSnapshotMessage.t
type output = DeleteDBSnapshotResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)