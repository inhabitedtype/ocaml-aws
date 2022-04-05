open Types
type input = DescribeInstanceTypeOfferingsRequest.t
type output = DescribeInstanceTypeOfferingsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error