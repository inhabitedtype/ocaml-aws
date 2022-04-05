open Types
type input = UpdateHostedZoneCommentRequest.t
type output = UpdateHostedZoneCommentResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error