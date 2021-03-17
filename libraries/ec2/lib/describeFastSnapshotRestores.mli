open Types
type input = DescribeFastSnapshotRestoresRequest.t
type output = DescribeFastSnapshotRestoresResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error