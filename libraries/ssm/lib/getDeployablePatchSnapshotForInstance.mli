open Types

type input = GetDeployablePatchSnapshotForInstanceRequest.t

type output = GetDeployablePatchSnapshotForInstanceResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
