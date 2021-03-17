open Types
type input = GetBucketLifecycleRequest.t
type output = GetBucketLifecycleOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error