open Aws.BaseTypes

type t =
  { device_name : String.t option
  ; virtual_name : String.t option
  ; ebs : EbsBlockDeviceResponse.t option
  ; no_device : String.t option
  }

let make ?device_name ?virtual_name ?ebs ?no_device () =
  { device_name; virtual_name; ebs; no_device }

let parse xml =
  Some
    { device_name = Aws.Util.option_bind (Aws.Xml.member "deviceName" xml) String.parse
    ; virtual_name = Aws.Util.option_bind (Aws.Xml.member "virtualName" xml) String.parse
    ; ebs = Aws.Util.option_bind (Aws.Xml.member "ebs" xml) EbsBlockDeviceResponse.parse
    ; no_device = Aws.Util.option_bind (Aws.Xml.member "noDevice" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.no_device (fun f ->
             Aws.Query.Pair ("NoDevice", String.to_query f))
       ; Aws.Util.option_map v.ebs (fun f ->
             Aws.Query.Pair ("Ebs", EbsBlockDeviceResponse.to_query f))
       ; Aws.Util.option_map v.virtual_name (fun f ->
             Aws.Query.Pair ("VirtualName", String.to_query f))
       ; Aws.Util.option_map v.device_name (fun f ->
             Aws.Query.Pair ("DeviceName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.no_device (fun f -> "noDevice", String.to_json f)
       ; Aws.Util.option_map v.ebs (fun f -> "ebs", EbsBlockDeviceResponse.to_json f)
       ; Aws.Util.option_map v.virtual_name (fun f -> "virtualName", String.to_json f)
       ; Aws.Util.option_map v.device_name (fun f -> "deviceName", String.to_json f)
       ])

let of_json j =
  { device_name = Aws.Util.option_map (Aws.Json.lookup j "deviceName") String.of_json
  ; virtual_name = Aws.Util.option_map (Aws.Json.lookup j "virtualName") String.of_json
  ; ebs = Aws.Util.option_map (Aws.Json.lookup j "ebs") EbsBlockDeviceResponse.of_json
  ; no_device = Aws.Util.option_map (Aws.Json.lookup j "noDevice") String.of_json
  }
