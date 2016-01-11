open Types
type input = AuthorizeCacheSecurityGroupIngressMessage.t
type output = AuthorizeCacheSecurityGroupIngressResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)