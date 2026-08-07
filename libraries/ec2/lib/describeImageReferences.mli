type input = DescribeImageReferencesRequest.t
type output = DescribeImageReferencesResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
