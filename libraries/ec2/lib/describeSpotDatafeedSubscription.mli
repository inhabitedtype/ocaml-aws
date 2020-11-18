open Types
type input = DescribeSpotDatafeedSubscriptionRequest.t
type output = DescribeSpotDatafeedSubscriptionResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error