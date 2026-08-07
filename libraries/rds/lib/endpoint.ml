open Aws.BaseTypes

type t =
  { address : String.t option
  ; port : Integer.t option
  ; hosted_zone_id : String.t option
  }

let make ?address ?port ?hosted_zone_id () = { address; port; hosted_zone_id }

let parse xml =
  Some
    { address = Aws.Util.option_bind (Aws.Xml.member "Address" xml) String.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; hosted_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.hosted_zone_id (fun f ->
             Aws.Query.Pair ("HostedZoneId", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.address (fun f ->
             Aws.Query.Pair ("Address", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.hosted_zone_id (fun f -> "HostedZoneId", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.address (fun f -> "Address", String.to_json f)
       ])

let of_json j =
  { address = Aws.Util.option_map (Aws.Json.lookup j "Address") String.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; hosted_zone_id = Aws.Util.option_map (Aws.Json.lookup j "HostedZoneId") String.of_json
  }
