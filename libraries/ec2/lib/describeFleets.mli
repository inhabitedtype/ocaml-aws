open Types
type input = DescribeFleetsRequest.t
type output = DescribeFleetsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error