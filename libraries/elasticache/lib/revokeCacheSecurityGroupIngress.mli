open Types
type input = RevokeCacheSecurityGroupIngressMessage.t
type output = RevokeCacheSecurityGroupIngressResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)