open Aws.BaseTypes

type t =
  { enabled : Boolean.t option
  ; delivery_status : VerifiedAccessLogDeliveryStatus.t option
  ; log_group : String.t option
  }

let make ?enabled ?delivery_status ?log_group () = { enabled; delivery_status; log_group }

let parse xml =
  Some
    { enabled = Aws.Util.option_bind (Aws.Xml.member "enabled" xml) Boolean.parse
    ; delivery_status =
        Aws.Util.option_bind
          (Aws.Xml.member "deliveryStatus" xml)
          VerifiedAccessLogDeliveryStatus.parse
    ; log_group = Aws.Util.option_bind (Aws.Xml.member "logGroup" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.log_group (fun f ->
             Aws.Query.Pair ("LogGroup", String.to_query f))
       ; Aws.Util.option_map v.delivery_status (fun f ->
             Aws.Query.Pair ("DeliveryStatus", VerifiedAccessLogDeliveryStatus.to_query f))
       ; Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.log_group (fun f -> "logGroup", String.to_json f)
       ; Aws.Util.option_map v.delivery_status (fun f ->
             "deliveryStatus", VerifiedAccessLogDeliveryStatus.to_json f)
       ; Aws.Util.option_map v.enabled (fun f -> "enabled", Boolean.to_json f)
       ])

let of_json j =
  { enabled = Aws.Util.option_map (Aws.Json.lookup j "enabled") Boolean.of_json
  ; delivery_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "deliveryStatus")
        VerifiedAccessLogDeliveryStatus.of_json
  ; log_group = Aws.Util.option_map (Aws.Json.lookup j "logGroup") String.of_json
  }
