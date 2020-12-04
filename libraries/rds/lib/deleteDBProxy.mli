open Types
type input = DeleteDBProxyRequest.t
type output = DeleteDBProxyResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error