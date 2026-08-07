open Aws.BaseTypes

type t =
  { group_name : String.t option
  ; state : PlacementGroupState.t option
  ; strategy : PlacementStrategy.t option
  ; partition_count : Integer.t option
  ; group_id : String.t option
  ; tags : TagList.t
  ; group_arn : String.t option
  ; spread_level : SpreadLevel.t option
  ; linked_group_id : String.t option
  ; operator : OperatorResponse.t option
  }

let make
    ?group_name
    ?state
    ?strategy
    ?partition_count
    ?group_id
    ?(tags = [])
    ?group_arn
    ?spread_level
    ?linked_group_id
    ?operator
    () =
  { group_name
  ; state
  ; strategy
  ; partition_count
  ; group_id
  ; tags
  ; group_arn
  ; spread_level
  ; linked_group_id
  ; operator
  }

let parse xml =
  Some
    { group_name = Aws.Util.option_bind (Aws.Xml.member "groupName" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) PlacementGroupState.parse
    ; strategy =
        Aws.Util.option_bind (Aws.Xml.member "strategy" xml) PlacementStrategy.parse
    ; partition_count =
        Aws.Util.option_bind (Aws.Xml.member "partitionCount" xml) Integer.parse
    ; group_id = Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; group_arn = Aws.Util.option_bind (Aws.Xml.member "groupArn" xml) String.parse
    ; spread_level =
        Aws.Util.option_bind (Aws.Xml.member "spreadLevel" xml) SpreadLevel.parse
    ; linked_group_id =
        Aws.Util.option_bind (Aws.Xml.member "linkedGroupId" xml) String.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Aws.Util.option_map v.linked_group_id (fun f ->
             Aws.Query.Pair ("LinkedGroupId", String.to_query f))
       ; Aws.Util.option_map v.spread_level (fun f ->
             Aws.Query.Pair ("SpreadLevel", SpreadLevel.to_query f))
       ; Aws.Util.option_map v.group_arn (fun f ->
             Aws.Query.Pair ("GroupArn", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ; Aws.Util.option_map v.partition_count (fun f ->
             Aws.Query.Pair ("PartitionCount", Integer.to_query f))
       ; Aws.Util.option_map v.strategy (fun f ->
             Aws.Query.Pair ("Strategy", PlacementStrategy.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", PlacementGroupState.to_query f))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Aws.Util.option_map v.linked_group_id (fun f ->
             "linkedGroupId", String.to_json f)
       ; Aws.Util.option_map v.spread_level (fun f ->
             "spreadLevel", SpreadLevel.to_json f)
       ; Aws.Util.option_map v.group_arn (fun f -> "groupArn", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.group_id (fun f -> "groupId", String.to_json f)
       ; Aws.Util.option_map v.partition_count (fun f ->
             "partitionCount", Integer.to_json f)
       ; Aws.Util.option_map v.strategy (fun f -> "strategy", PlacementStrategy.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", PlacementGroupState.to_json f)
       ; Aws.Util.option_map v.group_name (fun f -> "groupName", String.to_json f)
       ])

let of_json j =
  { group_name = Aws.Util.option_map (Aws.Json.lookup j "groupName") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") PlacementGroupState.of_json
  ; strategy =
      Aws.Util.option_map (Aws.Json.lookup j "strategy") PlacementStrategy.of_json
  ; partition_count =
      Aws.Util.option_map (Aws.Json.lookup j "partitionCount") Integer.of_json
  ; group_id = Aws.Util.option_map (Aws.Json.lookup j "groupId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; group_arn = Aws.Util.option_map (Aws.Json.lookup j "groupArn") String.of_json
  ; spread_level =
      Aws.Util.option_map (Aws.Json.lookup j "spreadLevel") SpreadLevel.of_json
  ; linked_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "linkedGroupId") String.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  }
