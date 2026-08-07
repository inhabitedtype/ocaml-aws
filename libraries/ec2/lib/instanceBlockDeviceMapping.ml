open Aws.BaseTypes

type t =
  { device_name : String.t option
  ; ebs : EbsInstanceBlockDevice.t option
  }

let make ?device_name ?ebs () = { device_name; ebs }

let parse xml =
  Some
    { device_name = Aws.Util.option_bind (Aws.Xml.member "deviceName" xml) String.parse
    ; ebs = Aws.Util.option_bind (Aws.Xml.member "ebs" xml) EbsInstanceBlockDevice.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ebs (fun f ->
             Aws.Query.Pair ("Ebs", EbsInstanceBlockDevice.to_query f))
       ; Aws.Util.option_map v.device_name (fun f ->
             Aws.Query.Pair ("DeviceName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ebs (fun f -> "ebs", EbsInstanceBlockDevice.to_json f)
       ; Aws.Util.option_map v.device_name (fun f -> "deviceName", String.to_json f)
       ])

let of_json j =
  { device_name = Aws.Util.option_map (Aws.Json.lookup j "deviceName") String.of_json
  ; ebs = Aws.Util.option_map (Aws.Json.lookup j "ebs") EbsInstanceBlockDevice.of_json
  }
