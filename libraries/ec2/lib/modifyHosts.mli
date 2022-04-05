open Types
type input = ModifyHostsRequest.t
type output = ModifyHostsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error