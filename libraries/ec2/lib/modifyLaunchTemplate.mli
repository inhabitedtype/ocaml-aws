open Types
type input = ModifyLaunchTemplateRequest.t
type output = ModifyLaunchTemplateResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error