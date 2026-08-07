open Aws.BaseTypes

type t =
  { network_id : String.t option
  ; network_type : AssociatedNetworkType.t option
  }

let make ?network_id ?network_type () = { network_id; network_type }

let parse xml =
  Some
    { network_id = Aws.Util.option_bind (Aws.Xml.member "networkId" xml) String.parse
    ; network_type =
        Aws.Util.option_bind
          (Aws.Xml.member "networkType" xml)
          AssociatedNetworkType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", AssociatedNetworkType.to_query f))
       ; Aws.Util.option_map v.network_id (fun f ->
             Aws.Query.Pair ("NetworkId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_type (fun f ->
             "networkType", AssociatedNetworkType.to_json f)
       ; Aws.Util.option_map v.network_id (fun f -> "networkId", String.to_json f)
       ])

let of_json j =
  { network_id = Aws.Util.option_map (Aws.Json.lookup j "networkId") String.of_json
  ; network_type =
      Aws.Util.option_map (Aws.Json.lookup j "networkType") AssociatedNetworkType.of_json
  }
