open Types

type input = DescribeParametersRequest.t

type output = DescribeParametersResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
