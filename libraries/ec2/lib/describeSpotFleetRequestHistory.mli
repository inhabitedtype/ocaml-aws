open Types
type input = DescribeSpotFleetRequestHistoryRequest.t
type output = DescribeSpotFleetRequestHistoryResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error