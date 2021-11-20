open Types
type input = ModifyTrafficMirrorSessionRequest.t
type output = ModifyTrafficMirrorSessionResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error