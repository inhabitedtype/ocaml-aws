open Types
type input = RevokeDBSecurityGroupIngressMessage.t
type output = RevokeDBSecurityGroupIngressResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)