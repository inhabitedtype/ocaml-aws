open Aws.BaseTypes

type t =
  { carrier_gateway_id : String.t option
  ; vpc_id : String.t option
  ; state : CarrierGatewayState.t option
  ; owner_id : String.t option
  ; tags : TagList.t
  }

let make ?carrier_gateway_id ?vpc_id ?state ?owner_id ?(tags = []) () =
  { carrier_gateway_id; vpc_id; state; owner_id; tags }

let parse xml =
  Some
    { carrier_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "carrierGatewayId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) CarrierGatewayState.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", CarrierGatewayState.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.carrier_gateway_id (fun f ->
             Aws.Query.Pair ("CarrierGatewayId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", CarrierGatewayState.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.carrier_gateway_id (fun f ->
             "carrierGatewayId", String.to_json f)
       ])

let of_json j =
  { carrier_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "carrierGatewayId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") CarrierGatewayState.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
