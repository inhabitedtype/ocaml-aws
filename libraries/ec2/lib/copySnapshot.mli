open Types
type input = CopySnapshotRequest.t
type output = CopySnapshotResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error