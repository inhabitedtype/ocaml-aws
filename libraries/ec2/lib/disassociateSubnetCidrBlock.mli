open Types
type input = DisassociateSubnetCidrBlockRequest.t
type output = DisassociateSubnetCidrBlockResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error