open Types
type input = ModifyCacheSubnetGroupMessage.t
type output = ModifyCacheSubnetGroupResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error