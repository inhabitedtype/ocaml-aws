open Types
type input = CreateDBSecurityGroupMessage.t
type output = CreateDBSecurityGroupResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)