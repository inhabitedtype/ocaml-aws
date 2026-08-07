open Aws.BaseTypes

type t =
  { activity_ids : ActivityIds.t
  ; auto_scaling_group_name : String.t option
  ; include_deleted_groups : Boolean.t option
  ; max_records : Integer.t option
  ; next_token : String.t option
  ; filters : Filters.t
  }

let make
    ?(activity_ids = [])
    ?auto_scaling_group_name
    ?include_deleted_groups
    ?max_records
    ?next_token
    ?(filters = [])
    () =
  { activity_ids
  ; auto_scaling_group_name
  ; include_deleted_groups
  ; max_records
  ; next_token
  ; filters
  }

let parse xml =
  Some
    { activity_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ActivityIds" xml) ActivityIds.parse)
    ; auto_scaling_group_name =
        Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
    ; include_deleted_groups =
        Aws.Util.option_bind (Aws.Xml.member "IncludeDeletedGroups" xml) Boolean.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) Filters.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filters.member", Filters.to_query v.filters))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.include_deleted_groups (fun f ->
             Aws.Query.Pair ("IncludeDeletedGroups", Boolean.to_query f))
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
       ; Some (Aws.Query.Pair ("ActivityIds.member", ActivityIds.to_query v.activity_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filters", Filters.to_json v.filters)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.include_deleted_groups (fun f ->
             "IncludeDeletedGroups", Boolean.to_json f)
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             "AutoScalingGroupName", String.to_json f)
       ; Some ("ActivityIds", ActivityIds.to_json v.activity_ids)
       ])

let of_json j =
  { activity_ids =
      ActivityIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ActivityIds"))
  ; auto_scaling_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
  ; include_deleted_groups =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeDeletedGroups") Boolean.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; filters = Filters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  }
