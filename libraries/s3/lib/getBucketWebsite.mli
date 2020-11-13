open Types
type input = GetBucketWebsiteRequest.t
type output = GetBucketWebsiteOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error