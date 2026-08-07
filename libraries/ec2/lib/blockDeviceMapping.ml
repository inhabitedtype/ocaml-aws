open Aws.BaseTypes

type t =
  { ebs : EbsBlockDevice.t option
  ; no_device : String.t option
  ; device_name : String.t
  ; virtual_name : String.t option
  }

let make ?ebs ?no_device ~device_name ?virtual_name () =
  { ebs; no_device; device_name; virtual_name }

let parse xml =
  Some
    { ebs = Aws.Util.option_bind (Aws.Xml.member "ebs" xml) EbsBlockDevice.parse
    ; no_device = Aws.Util.option_bind (Aws.Xml.member "noDevice" xml) String.parse
    ; device_name =
        Aws.Xml.required
          "deviceName"
          (Aws.Util.option_bind (Aws.Xml.member "deviceName" xml) String.parse)
    ; virtual_name = Aws.Util.option_bind (Aws.Xml.member "virtualName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.virtual_name (fun f ->
             Aws.Query.Pair ("VirtualName", String.to_query f))
       ; Some (Aws.Query.Pair ("DeviceName", String.to_query v.device_name))
       ; Aws.Util.option_map v.no_device (fun f ->
             Aws.Query.Pair ("NoDevice", String.to_query f))
       ; Aws.Util.option_map v.ebs (fun f ->
             Aws.Query.Pair ("Ebs", EbsBlockDevice.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.virtual_name (fun f -> "virtualName", String.to_json f)
       ; Some ("deviceName", String.to_json v.device_name)
       ; Aws.Util.option_map v.no_device (fun f -> "noDevice", String.to_json f)
       ; Aws.Util.option_map v.ebs (fun f -> "ebs", EbsBlockDevice.to_json f)
       ])

let of_json j =
  { ebs = Aws.Util.option_map (Aws.Json.lookup j "ebs") EbsBlockDevice.of_json
  ; no_device = Aws.Util.option_map (Aws.Json.lookup j "noDevice") String.of_json
  ; device_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "deviceName"))
  ; virtual_name = Aws.Util.option_map (Aws.Json.lookup j "virtualName") String.of_json
  }
