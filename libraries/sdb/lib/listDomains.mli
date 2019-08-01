open Types
type input = ListDomainsRequest.t
type output = ListDomainsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error