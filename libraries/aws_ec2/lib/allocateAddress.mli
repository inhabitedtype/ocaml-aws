open Types
type input = AllocateAddressRequest.t
type output = AllocateAddressResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)