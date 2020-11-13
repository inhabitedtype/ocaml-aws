open Types
type input = DecodeAuthorizationMessageRequest.t
type output = DecodeAuthorizationMessageResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error