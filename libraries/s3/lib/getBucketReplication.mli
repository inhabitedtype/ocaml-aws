open Types
type input = GetBucketReplicationRequest.t
type output = GetBucketReplicationOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error