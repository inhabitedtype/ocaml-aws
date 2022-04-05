open Types
type input = DescribeLaunchTemplatesRequest.t
type output = DescribeLaunchTemplatesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error