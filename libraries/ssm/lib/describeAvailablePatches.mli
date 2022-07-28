open Types

type input = DescribeAvailablePatchesRequest.t

type output = DescribeAvailablePatchesResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
