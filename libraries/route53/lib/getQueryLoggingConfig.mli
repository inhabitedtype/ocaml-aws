open Types
type input = GetQueryLoggingConfigRequest.t
type output = GetQueryLoggingConfigResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error