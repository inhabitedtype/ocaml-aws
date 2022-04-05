open Types
type input = UpdateTrafficPolicyCommentRequest.t
type output = UpdateTrafficPolicyCommentResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error