open Types
type input = ListTrafficPolicyInstancesByPolicyRequest.t
type output = ListTrafficPolicyInstancesByPolicyResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error