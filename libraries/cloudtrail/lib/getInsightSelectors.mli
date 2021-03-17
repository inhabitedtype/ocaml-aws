open Types
type input = GetInsightSelectorsRequest.t
type output = GetInsightSelectorsResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error