open Types
type input = GetObjectAclRequest.t
type output = GetObjectAclOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error