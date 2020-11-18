open Types
type input = GetManagedScalingPolicyInput.t
type output = GetManagedScalingPolicyOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error