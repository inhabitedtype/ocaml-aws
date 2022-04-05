open Types
type input = DescribeInstanceEventNotificationAttributesRequest.t
type output = DescribeInstanceEventNotificationAttributesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error