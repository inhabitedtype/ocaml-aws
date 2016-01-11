open Types
type input = AttachVolumeRequest.t
type output = VolumeAttachment.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)