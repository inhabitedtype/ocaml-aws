open Aws.BaseTypes

type t =
  { exclusion_id : String.t option
  ; internet_gateway_exclusion_mode : InternetGatewayExclusionMode.t option
  ; resource_arn : String.t option
  ; state : VpcBlockPublicAccessExclusionState.t option
  ; reason : String.t option
  ; creation_timestamp : DateTime.t option
  ; last_update_timestamp : DateTime.t option
  ; deletion_timestamp : DateTime.t option
  ; tags : TagList.t
  }

let make
    ?exclusion_id
    ?internet_gateway_exclusion_mode
    ?resource_arn
    ?state
    ?reason
    ?creation_timestamp
    ?last_update_timestamp
    ?deletion_timestamp
    ?(tags = [])
    () =
  { exclusion_id
  ; internet_gateway_exclusion_mode
  ; resource_arn
  ; state
  ; reason
  ; creation_timestamp
  ; last_update_timestamp
  ; deletion_timestamp
  ; tags
  }

let parse xml =
  Some
    { exclusion_id = Aws.Util.option_bind (Aws.Xml.member "exclusionId" xml) String.parse
    ; internet_gateway_exclusion_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "internetGatewayExclusionMode" xml)
          InternetGatewayExclusionMode.parse
    ; resource_arn = Aws.Util.option_bind (Aws.Xml.member "resourceArn" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          VpcBlockPublicAccessExclusionState.parse
    ; reason = Aws.Util.option_bind (Aws.Xml.member "reason" xml) String.parse
    ; creation_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "creationTimestamp" xml) DateTime.parse
    ; last_update_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "lastUpdateTimestamp" xml) DateTime.parse
    ; deletion_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "deletionTimestamp" xml) DateTime.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.deletion_timestamp (fun f ->
             Aws.Query.Pair ("DeletionTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.last_update_timestamp (fun f ->
             Aws.Query.Pair ("LastUpdateTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.creation_timestamp (fun f ->
             Aws.Query.Pair ("CreationTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.reason (fun f ->
             Aws.Query.Pair ("Reason", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", VpcBlockPublicAccessExclusionState.to_query f))
       ; Aws.Util.option_map v.resource_arn (fun f ->
             Aws.Query.Pair ("ResourceArn", String.to_query f))
       ; Aws.Util.option_map v.internet_gateway_exclusion_mode (fun f ->
             Aws.Query.Pair
               ("InternetGatewayExclusionMode", InternetGatewayExclusionMode.to_query f))
       ; Aws.Util.option_map v.exclusion_id (fun f ->
             Aws.Query.Pair ("ExclusionId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.deletion_timestamp (fun f ->
             "deletionTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.last_update_timestamp (fun f ->
             "lastUpdateTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.creation_timestamp (fun f ->
             "creationTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.reason (fun f -> "reason", String.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", VpcBlockPublicAccessExclusionState.to_json f)
       ; Aws.Util.option_map v.resource_arn (fun f -> "resourceArn", String.to_json f)
       ; Aws.Util.option_map v.internet_gateway_exclusion_mode (fun f ->
             "internetGatewayExclusionMode", InternetGatewayExclusionMode.to_json f)
       ; Aws.Util.option_map v.exclusion_id (fun f -> "exclusionId", String.to_json f)
       ])

let of_json j =
  { exclusion_id = Aws.Util.option_map (Aws.Json.lookup j "exclusionId") String.of_json
  ; internet_gateway_exclusion_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "internetGatewayExclusionMode")
        InternetGatewayExclusionMode.of_json
  ; resource_arn = Aws.Util.option_map (Aws.Json.lookup j "resourceArn") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        VpcBlockPublicAccessExclusionState.of_json
  ; reason = Aws.Util.option_map (Aws.Json.lookup j "reason") String.of_json
  ; creation_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "creationTimestamp") DateTime.of_json
  ; last_update_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "lastUpdateTimestamp") DateTime.of_json
  ; deletion_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "deletionTimestamp") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
