open Types

type input = ListKeyPoliciesRequest.t

type output = ListKeyPoliciesResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
