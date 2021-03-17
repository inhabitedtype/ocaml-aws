open Types
type input = DisableFastSnapshotRestoresRequest.t
type output = DisableFastSnapshotRestoresResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error