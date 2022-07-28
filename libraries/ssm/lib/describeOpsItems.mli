open Types

type input = DescribeOpsItemsRequest.t

type output = DescribeOpsItemsResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
