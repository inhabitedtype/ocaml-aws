open Aws.BaseTypes

type t =
  { destination_cidr_block : String.t option
  ; source : VpnStaticRouteSource.t option
  ; state : VpnState.t option
  }

let make ?destination_cidr_block ?source ?state () =
  { destination_cidr_block; source; state }

let parse xml =
  Some
    { destination_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "destinationCidrBlock" xml) String.parse
    ; source =
        Aws.Util.option_bind (Aws.Xml.member "source" xml) VpnStaticRouteSource.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) VpnState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", VpnState.to_query f))
       ; Aws.Util.option_map v.source (fun f ->
             Aws.Query.Pair ("Source", VpnStaticRouteSource.to_query f))
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationCidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f -> "state", VpnState.to_json f)
       ; Aws.Util.option_map v.source (fun f -> "source", VpnStaticRouteSource.to_json f)
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             "destinationCidrBlock", String.to_json f)
       ])

let of_json j =
  { destination_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "destinationCidrBlock") String.of_json
  ; source = Aws.Util.option_map (Aws.Json.lookup j "source") VpnStaticRouteSource.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") VpnState.of_json
  }
