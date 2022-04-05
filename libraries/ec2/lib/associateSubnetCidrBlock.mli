open Types
type input = AssociateSubnetCidrBlockRequest.t
type output = AssociateSubnetCidrBlockResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error