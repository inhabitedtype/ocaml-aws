open Types
type input = UpdateSecurityGroupRuleDescriptionsIngressRequest.t
type output = UpdateSecurityGroupRuleDescriptionsIngressResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error