open Types
type input = CreateDhcpOptionsRequest.t
type output = CreateDhcpOptionsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error