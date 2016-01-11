open Types
type input = DescribeBundleTasksRequest.t
type output = DescribeBundleTasksResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)