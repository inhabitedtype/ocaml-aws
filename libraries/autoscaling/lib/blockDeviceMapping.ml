open Aws.BaseTypes

type t =
  { virtual_name : String.t option
  ; device_name : String.t
  ; ebs : Ebs.t option
  ; no_device : Boolean.t option
  }

let make ?virtual_name ~device_name ?ebs ?no_device () =
  { virtual_name; device_name; ebs; no_device }

let parse xml =
  Some
    { virtual_name = Aws.Util.option_bind (Aws.Xml.member "VirtualName" xml) String.parse
    ; device_name =
        Aws.Xml.required
          "DeviceName"
          (Aws.Util.option_bind (Aws.Xml.member "DeviceName" xml) String.parse)
    ; ebs = Aws.Util.option_bind (Aws.Xml.member "Ebs" xml) Ebs.parse
    ; no_device = Aws.Util.option_bind (Aws.Xml.member "NoDevice" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.no_device (fun f ->
             Aws.Query.Pair ("NoDevice", Boolean.to_query f))
       ; Aws.Util.option_map v.ebs (fun f -> Aws.Query.Pair ("Ebs", Ebs.to_query f))
       ; Some (Aws.Query.Pair ("DeviceName", String.to_query v.device_name))
       ; Aws.Util.option_map v.virtual_name (fun f ->
             Aws.Query.Pair ("VirtualName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.no_device (fun f -> "NoDevice", Boolean.to_json f)
       ; Aws.Util.option_map v.ebs (fun f -> "Ebs", Ebs.to_json f)
       ; Some ("DeviceName", String.to_json v.device_name)
       ; Aws.Util.option_map v.virtual_name (fun f -> "VirtualName", String.to_json f)
       ])

let of_json j =
  { virtual_name = Aws.Util.option_map (Aws.Json.lookup j "VirtualName") String.of_json
  ; device_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DeviceName"))
  ; ebs = Aws.Util.option_map (Aws.Json.lookup j "Ebs") Ebs.of_json
  ; no_device = Aws.Util.option_map (Aws.Json.lookup j "NoDevice") Boolean.of_json
  }
