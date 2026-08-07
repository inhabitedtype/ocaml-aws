open Aws.BaseTypes

type t =
  { transit_gateway_id : String.t option
  ; transit_gateway_arn : String.t option
  ; state : TransitGatewayState.t option
  ; owner_id : String.t option
  ; description : String.t option
  ; creation_time : DateTime.t option
  ; options : TransitGatewayOptions.t option
  ; tags : TagList.t
  }

let make
    ?transit_gateway_id
    ?transit_gateway_arn
    ?state
    ?owner_id
    ?description
    ?creation_time
    ?options
    ?(tags = [])
    () =
  { transit_gateway_id
  ; transit_gateway_arn
  ; state
  ; owner_id
  ; description
  ; creation_time
  ; options
  ; tags
  }

let parse xml =
  Some
    { transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; transit_gateway_arn =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayArn" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) TransitGatewayState.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) DateTime.parse
    ; options =
        Aws.Util.option_bind (Aws.Xml.member "options" xml) TransitGatewayOptions.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.options (fun f ->
             Aws.Query.Pair ("Options", TransitGatewayOptions.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayState.to_query f))
       ; Aws.Util.option_map v.transit_gateway_arn (fun f ->
             Aws.Query.Pair ("TransitGatewayArn", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.options (fun f ->
             "options", TransitGatewayOptions.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", TransitGatewayState.to_json f)
       ; Aws.Util.option_map v.transit_gateway_arn (fun f ->
             "transitGatewayArn", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; transit_gateway_arn =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayArn") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") TransitGatewayState.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "creationTime") DateTime.of_json
  ; options =
      Aws.Util.option_map (Aws.Json.lookup j "options") TransitGatewayOptions.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
