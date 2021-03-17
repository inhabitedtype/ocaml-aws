open Types
type input = DeleteTrafficMirrorFilterRequest.t
type output = DeleteTrafficMirrorFilterResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error