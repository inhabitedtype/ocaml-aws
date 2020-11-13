open Types
type input = GetBucketTaggingRequest.t
type output = GetBucketTaggingOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error