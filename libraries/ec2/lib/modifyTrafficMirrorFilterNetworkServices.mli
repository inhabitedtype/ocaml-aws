open Types
type input = ModifyTrafficMirrorFilterNetworkServicesRequest.t
type output = ModifyTrafficMirrorFilterNetworkServicesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error