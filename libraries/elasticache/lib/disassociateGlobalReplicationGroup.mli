open Types
type input = DisassociateGlobalReplicationGroupMessage.t
type output = DisassociateGlobalReplicationGroupResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error