open Types
type input = CreateNetworkAclRequest.t
type output = CreateNetworkAclResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)