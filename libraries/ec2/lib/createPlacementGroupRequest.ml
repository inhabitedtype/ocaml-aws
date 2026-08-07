open Aws.BaseTypes

type t =
  { partition_count : Integer.t option
  ; tag_specifications : TagSpecificationList.t
  ; spread_level : SpreadLevel.t option
  ; linked_group_id : String.t option
  ; operator : OperatorRequest.t option
  ; dry_run : Boolean.t option
  ; group_name : String.t option
  ; strategy : PlacementStrategy.t option
  }

let make
    ?partition_count
    ?(tag_specifications = [])
    ?spread_level
    ?linked_group_id
    ?operator
    ?dry_run
    ?group_name
    ?strategy
    () =
  { partition_count
  ; tag_specifications
  ; spread_level
  ; linked_group_id
  ; operator
  ; dry_run
  ; group_name
  ; strategy
  }

let parse xml =
  Some
    { partition_count =
        Aws.Util.option_bind (Aws.Xml.member "PartitionCount" xml) Integer.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; spread_level =
        Aws.Util.option_bind (Aws.Xml.member "SpreadLevel" xml) SpreadLevel.parse
    ; linked_group_id =
        Aws.Util.option_bind (Aws.Xml.member "LinkedGroupId" xml) String.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "Operator" xml) OperatorRequest.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "groupName" xml) String.parse
    ; strategy =
        Aws.Util.option_bind (Aws.Xml.member "strategy" xml) PlacementStrategy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.strategy (fun f ->
             Aws.Query.Pair ("Strategy", PlacementStrategy.to_query f))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorRequest.to_query f))
       ; Aws.Util.option_map v.linked_group_id (fun f ->
             Aws.Query.Pair ("LinkedGroupId", String.to_query f))
       ; Aws.Util.option_map v.spread_level (fun f ->
             Aws.Query.Pair ("SpreadLevel", SpreadLevel.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.partition_count (fun f ->
             Aws.Query.Pair ("PartitionCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.strategy (fun f -> "strategy", PlacementStrategy.to_json f)
       ; Aws.Util.option_map v.group_name (fun f -> "groupName", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.operator (fun f -> "Operator", OperatorRequest.to_json f)
       ; Aws.Util.option_map v.linked_group_id (fun f ->
             "LinkedGroupId", String.to_json f)
       ; Aws.Util.option_map v.spread_level (fun f ->
             "SpreadLevel", SpreadLevel.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.partition_count (fun f ->
             "PartitionCount", Integer.to_json f)
       ])

let of_json j =
  { partition_count =
      Aws.Util.option_map (Aws.Json.lookup j "PartitionCount") Integer.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; spread_level =
      Aws.Util.option_map (Aws.Json.lookup j "SpreadLevel") SpreadLevel.of_json
  ; linked_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "LinkedGroupId") String.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "Operator") OperatorRequest.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "groupName") String.of_json
  ; strategy =
      Aws.Util.option_map (Aws.Json.lookup j "strategy") PlacementStrategy.of_json
  }
