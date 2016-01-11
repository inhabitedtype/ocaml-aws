open Types
type input = CreateCacheSecurityGroupMessage.t
type output = CreateCacheSecurityGroupResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)