open Types
type input = DescribeSnapshotsRequest.t
type output = DescribeSnapshotsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error