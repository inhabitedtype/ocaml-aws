open Types
type input = DescribeSpotFleetRequestsRequest.t
type output = DescribeSpotFleetRequestsResponse.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)