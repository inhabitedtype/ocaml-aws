open Types
type input = ModifyLoadBalancerAttributesInput.t
type output = ModifyLoadBalancerAttributesOutput.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)