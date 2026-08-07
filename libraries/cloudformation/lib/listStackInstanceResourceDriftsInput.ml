open Aws.BaseTypes

type t =
  { stack_set_name : String.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; stack_instance_resource_drift_statuses : StackResourceDriftStatusFilters.t
  ; stack_instance_account : String.t
  ; stack_instance_region : String.t
  ; operation_id : String.t
  ; call_as : CallAs.t option
  }

let make
    ~stack_set_name
    ?next_token
    ?max_results
    ?(stack_instance_resource_drift_statuses = [])
    ~stack_instance_account
    ~stack_instance_region
    ~operation_id
    ?call_as
    () =
  { stack_set_name
  ; next_token
  ; max_results
  ; stack_instance_resource_drift_statuses
  ; stack_instance_account
  ; stack_instance_region
  ; operation_id
  ; call_as
  }

let parse xml =
  Some
    { stack_set_name =
        Aws.Xml.required
          "StackSetName"
          (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; stack_instance_resource_drift_statuses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "StackInstanceResourceDriftStatuses" xml)
             StackResourceDriftStatusFilters.parse)
    ; stack_instance_account =
        Aws.Xml.required
          "StackInstanceAccount"
          (Aws.Util.option_bind (Aws.Xml.member "StackInstanceAccount" xml) String.parse)
    ; stack_instance_region =
        Aws.Xml.required
          "StackInstanceRegion"
          (Aws.Util.option_bind (Aws.Xml.member "StackInstanceRegion" xml) String.parse)
    ; operation_id =
        Aws.Xml.required
          "OperationId"
          (Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse)
    ; call_as = Aws.Util.option_bind (Aws.Xml.member "CallAs" xml) CallAs.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f ->
             Aws.Query.Pair ("CallAs", CallAs.to_query f))
       ; Some (Aws.Query.Pair ("OperationId", String.to_query v.operation_id))
       ; Some
           (Aws.Query.Pair ("StackInstanceRegion", String.to_query v.stack_instance_region))
       ; Some
           (Aws.Query.Pair
              ("StackInstanceAccount", String.to_query v.stack_instance_account))
       ; Some
           (Aws.Query.Pair
              ( "StackInstanceResourceDriftStatuses.member"
              , StackResourceDriftStatusFilters.to_query
                  v.stack_instance_resource_drift_statuses ))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f -> "CallAs", CallAs.to_json f)
       ; Some ("OperationId", String.to_json v.operation_id)
       ; Some ("StackInstanceRegion", String.to_json v.stack_instance_region)
       ; Some ("StackInstanceAccount", String.to_json v.stack_instance_account)
       ; Some
           ( "StackInstanceResourceDriftStatuses"
           , StackResourceDriftStatusFilters.to_json
               v.stack_instance_resource_drift_statuses )
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("StackSetName", String.to_json v.stack_set_name)
       ])

let of_json j =
  { stack_set_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; stack_instance_resource_drift_statuses =
      StackResourceDriftStatusFilters.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StackInstanceResourceDriftStatuses"))
  ; stack_instance_account =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackInstanceAccount"))
  ; stack_instance_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackInstanceRegion"))
  ; operation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OperationId"))
  ; call_as = Aws.Util.option_map (Aws.Json.lookup j "CallAs") CallAs.of_json
  }
