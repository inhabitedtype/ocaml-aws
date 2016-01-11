open Types
type input = AttachLoadBalancerToSubnetsInput.t
type output = AttachLoadBalancerToSubnetsOutput.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)