open Aws.BaseTypes

type t =
  { auto_scaling_group_names : AutoScalingGroupNames.t
  ; include_instances : Boolean.t option
  ; next_token : String.t option
  ; max_records : Integer.t option
  ; filters : Filters.t
  }

let make
    ?(auto_scaling_group_names = [])
    ?include_instances
    ?next_token
    ?max_records
    ?(filters = [])
    () =
  { auto_scaling_group_names; include_instances; next_token; max_records; filters }

let parse xml =
  Some
    { auto_scaling_group_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AutoScalingGroupNames" xml)
             AutoScalingGroupNames.parse)
    ; include_instances =
        Aws.Util.option_bind (Aws.Xml.member "IncludeInstances" xml) Boolean.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) Filters.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filters.member", Filters.to_query v.filters))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.include_instances (fun f ->
             Aws.Query.Pair ("IncludeInstances", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AutoScalingGroupNames.member"
              , AutoScalingGroupNames.to_query v.auto_scaling_group_names ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filters", Filters.to_json v.filters)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.include_instances (fun f ->
             "IncludeInstances", Boolean.to_json f)
       ; Some
           ( "AutoScalingGroupNames"
           , AutoScalingGroupNames.to_json v.auto_scaling_group_names )
       ])

let of_json j =
  { auto_scaling_group_names =
      AutoScalingGroupNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupNames"))
  ; include_instances =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeInstances") Boolean.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; filters = Filters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  }
