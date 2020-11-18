open Types
type input = AssociateAddressRequest.t
type output = AssociateAddressResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error