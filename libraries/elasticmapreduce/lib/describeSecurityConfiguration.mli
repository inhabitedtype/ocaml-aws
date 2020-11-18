open Types
type input = DescribeSecurityConfigurationInput.t
type output = DescribeSecurityConfigurationOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error