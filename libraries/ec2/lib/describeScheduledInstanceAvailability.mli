open Types
type input = DescribeScheduledInstanceAvailabilityRequest.t
type output = DescribeScheduledInstanceAvailabilityResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error