open Types
type input = CreateSnapshotsRequest.t
type output = CreateSnapshotsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error