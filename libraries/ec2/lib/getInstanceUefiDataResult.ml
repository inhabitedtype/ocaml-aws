open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; uefi_data : String.t option
  }

let make ?instance_id ?uefi_data () = { instance_id; uefi_data }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; uefi_data = Aws.Util.option_bind (Aws.Xml.member "uefiData" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.uefi_data (fun f ->
             Aws.Query.Pair ("UefiData", String.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.uefi_data (fun f -> "uefiData", String.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; uefi_data = Aws.Util.option_map (Aws.Json.lookup j "uefiData") String.of_json
  }
