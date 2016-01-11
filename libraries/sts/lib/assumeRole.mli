open Types
type input = AssumeRoleRequest.t
type output = AssumeRoleResponse.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)