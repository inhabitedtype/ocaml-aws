open Types
type input = CreateDBProxyRequest.t
type output = CreateDBProxyResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error