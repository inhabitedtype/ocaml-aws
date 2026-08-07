open Aws.BaseTypes

type t =
  { device_name : String.t option
  ; virtual_name : String.t option
  ; ebs : LaunchTemplateEbsBlockDeviceRequest.t option
  ; no_device : String.t option
  }

let make ?device_name ?virtual_name ?ebs ?no_device () =
  { device_name; virtual_name; ebs; no_device }

let parse xml =
  Some
    { device_name = Aws.Util.option_bind (Aws.Xml.member "DeviceName" xml) String.parse
    ; virtual_name = Aws.Util.option_bind (Aws.Xml.member "VirtualName" xml) String.parse
    ; ebs =
        Aws.Util.option_bind
          (Aws.Xml.member "Ebs" xml)
          LaunchTemplateEbsBlockDeviceRequest.parse
    ; no_device = Aws.Util.option_bind (Aws.Xml.member "NoDevice" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.no_device (fun f ->
             Aws.Query.Pair ("NoDevice", String.to_query f))
       ; Aws.Util.option_map v.ebs (fun f ->
             Aws.Query.Pair ("Ebs", LaunchTemplateEbsBlockDeviceRequest.to_query f))
       ; Aws.Util.option_map v.virtual_name (fun f ->
             Aws.Query.Pair ("VirtualName", String.to_query f))
       ; Aws.Util.option_map v.device_name (fun f ->
             Aws.Query.Pair ("DeviceName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.no_device (fun f -> "NoDevice", String.to_json f)
       ; Aws.Util.option_map v.ebs (fun f ->
             "Ebs", LaunchTemplateEbsBlockDeviceRequest.to_json f)
       ; Aws.Util.option_map v.virtual_name (fun f -> "VirtualName", String.to_json f)
       ; Aws.Util.option_map v.device_name (fun f -> "DeviceName", String.to_json f)
       ])

let of_json j =
  { device_name = Aws.Util.option_map (Aws.Json.lookup j "DeviceName") String.of_json
  ; virtual_name = Aws.Util.option_map (Aws.Json.lookup j "VirtualName") String.of_json
  ; ebs =
      Aws.Util.option_map
        (Aws.Json.lookup j "Ebs")
        LaunchTemplateEbsBlockDeviceRequest.of_json
  ; no_device = Aws.Util.option_map (Aws.Json.lookup j "NoDevice") String.of_json
  }
