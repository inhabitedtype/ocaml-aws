open Types
type input = DescribeDhcpOptionsRequest.t
type output = DescribeDhcpOptionsResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)