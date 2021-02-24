open Types
type input = GetAccessKeyInfoRequest.t
type output = GetAccessKeyInfoResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error