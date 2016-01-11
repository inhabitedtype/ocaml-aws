open Types
type input = DescribeSpotPriceHistoryRequest.t
type output = DescribeSpotPriceHistoryResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)