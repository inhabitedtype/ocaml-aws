open Types
type input = CreateTrafficMirrorFilterRuleRequest.t
type output = CreateTrafficMirrorFilterRuleResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error