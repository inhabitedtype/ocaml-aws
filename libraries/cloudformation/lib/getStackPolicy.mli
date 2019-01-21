open Types_internal
type input = GetStackPolicyInput.t
type output = GetStackPolicyOutput.t
type error = Errors_internal.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)