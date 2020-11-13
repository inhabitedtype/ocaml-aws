open Types
type input = AttachClassicLinkVpcRequest.t
type output = AttachClassicLinkVpcResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error