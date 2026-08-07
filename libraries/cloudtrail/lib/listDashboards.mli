type input = ListDashboardsRequest.t
type output = ListDashboardsResponse.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
