open Types

type input = DescribeInstancePropertiesRequest.t
type output = DescribeInstancePropertiesResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
