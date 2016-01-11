open Types
type input = DescribeSpotFleetInstancesRequest.t
type output = DescribeSpotFleetInstancesResponse.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)