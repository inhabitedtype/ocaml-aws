open Types
type input = DeleteLaunchTemplateRequest.t
type output = DeleteLaunchTemplateResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error