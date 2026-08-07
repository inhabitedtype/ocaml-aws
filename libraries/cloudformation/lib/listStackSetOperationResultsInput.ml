open Aws.BaseTypes

type t =
  { stack_set_name : String.t
  ; operation_id : String.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; call_as : CallAs.t option
  ; filters : OperationResultFilters.t
  }

let make
    ~stack_set_name
    ~operation_id
    ?next_token
    ?max_results
    ?call_as
    ?(filters = [])
    () =
  { stack_set_name; operation_id; next_token; max_results; call_as; filters }

let parse xml =
  Some
    { stack_set_name =
        Aws.Xml.required
          "StackSetName"
          (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
    ; operation_id =
        Aws.Xml.required
          "OperationId"
          (Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; call_as = Aws.Util.option_bind (Aws.Xml.member "CallAs" xml) CallAs.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Filters" xml)
             OperationResultFilters.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Filters.member", OperationResultFilters.to_query v.filters))
       ; Aws.Util.option_map v.call_as (fun f ->
             Aws.Query.Pair ("CallAs", CallAs.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("OperationId", String.to_query v.operation_id))
       ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filters", OperationResultFilters.to_json v.filters)
       ; Aws.Util.option_map v.call_as (fun f -> "CallAs", CallAs.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("OperationId", String.to_json v.operation_id)
       ; Some ("StackSetName", String.to_json v.stack_set_name)
       ])

let of_json j =
  { stack_set_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
  ; operation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OperationId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; call_as = Aws.Util.option_map (Aws.Json.lookup j "CallAs") CallAs.of_json
  ; filters =
      OperationResultFilters.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  }
