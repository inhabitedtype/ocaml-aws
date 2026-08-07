open Aws.BaseTypes

type t =
  { stack_set_name : String.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; filters : StackInstanceFilters.t
  ; stack_instance_account : String.t option
  ; stack_instance_region : String.t option
  ; call_as : CallAs.t option
  }

let make
    ~stack_set_name
    ?next_token
    ?max_results
    ?(filters = [])
    ?stack_instance_account
    ?stack_instance_region
    ?call_as
    () =
  { stack_set_name
  ; next_token
  ; max_results
  ; filters
  ; stack_instance_account
  ; stack_instance_region
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
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) StackInstanceFilters.parse)
    ; stack_instance_account =
        Aws.Util.option_bind (Aws.Xml.member "StackInstanceAccount" xml) String.parse
    ; stack_instance_region =
        Aws.Util.option_bind (Aws.Xml.member "StackInstanceRegion" xml) String.parse
    ; call_as = Aws.Util.option_bind (Aws.Xml.member "CallAs" xml) CallAs.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f ->
             Aws.Query.Pair ("CallAs", CallAs.to_query f))
       ; Aws.Util.option_map v.stack_instance_region (fun f ->
             Aws.Query.Pair ("StackInstanceRegion", String.to_query f))
       ; Aws.Util.option_map v.stack_instance_account (fun f ->
             Aws.Query.Pair ("StackInstanceAccount", String.to_query f))
       ; Some (Aws.Query.Pair ("Filters.member", StackInstanceFilters.to_query v.filters))
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
       ; Aws.Util.option_map v.stack_instance_region (fun f ->
             "StackInstanceRegion", String.to_json f)
       ; Aws.Util.option_map v.stack_instance_account (fun f ->
             "StackInstanceAccount", String.to_json f)
       ; Some ("Filters", StackInstanceFilters.to_json v.filters)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("StackSetName", String.to_json v.stack_set_name)
       ])

let of_json j =
  { stack_set_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; filters =
      StackInstanceFilters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; stack_instance_account =
      Aws.Util.option_map (Aws.Json.lookup j "StackInstanceAccount") String.of_json
  ; stack_instance_region =
      Aws.Util.option_map (Aws.Json.lookup j "StackInstanceRegion") String.of_json
  ; call_as = Aws.Util.option_map (Aws.Json.lookup j "CallAs") CallAs.of_json
  }
