open Types
type input = LaunchConfigurationNamesType.t
type output = LaunchConfigurationsType.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error