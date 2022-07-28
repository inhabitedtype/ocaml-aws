open Types

type input = DescribeDocumentPermissionRequest.t

type output = DescribeDocumentPermissionResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
