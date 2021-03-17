open Types
type input = DeleteTrafficMirrorFilterRuleRequest.t
type output = DeleteTrafficMirrorFilterRuleResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error