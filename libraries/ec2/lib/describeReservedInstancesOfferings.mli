open Types
type input = DescribeReservedInstancesOfferingsRequest.t
type output = DescribeReservedInstancesOfferingsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error