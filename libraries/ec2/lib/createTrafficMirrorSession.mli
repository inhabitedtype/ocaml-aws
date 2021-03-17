open Types
type input = CreateTrafficMirrorSessionRequest.t
type output = CreateTrafficMirrorSessionResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error