open Aws.BaseTypes

type t =
  { stack_name : String.t
  ; stack_resource_drift_status_filters : StackResourceDriftStatusFilters.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make
    ~stack_name
    ?(stack_resource_drift_status_filters = [])
    ?next_token
    ?max_results
    () =
  { stack_name; stack_resource_drift_status_filters; next_token; max_results }

let parse xml =
  Some
    { stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
    ; stack_resource_drift_status_filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "StackResourceDriftStatusFilters" xml)
             StackResourceDriftStatusFilters.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "StackResourceDriftStatusFilters.member"
              , StackResourceDriftStatusFilters.to_query
                  v.stack_resource_drift_status_filters ))
       ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "StackResourceDriftStatusFilters"
           , StackResourceDriftStatusFilters.to_json v.stack_resource_drift_status_filters
           )
       ; Some ("StackName", String.to_json v.stack_name)
       ])

let of_json j =
  { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  ; stack_resource_drift_status_filters =
      StackResourceDriftStatusFilters.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceDriftStatusFilters"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
