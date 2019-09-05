open Types

type input = ListSecurityConfigurationsInput.t

type output = ListSecurityConfigurationsOutput.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
