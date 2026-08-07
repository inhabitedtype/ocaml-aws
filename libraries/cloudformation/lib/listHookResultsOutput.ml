open Aws.BaseTypes

type t =
  { target_type : ListHookResultsTargetType.t option
  ; target_id : String.t option
  ; hook_results : HookResultSummaries.t
  ; next_token : String.t option
  }

let make ?target_type ?target_id ?(hook_results = []) ?next_token () =
  { target_type; target_id; hook_results; next_token }

let parse xml =
  Some
    { target_type =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetType" xml)
          ListHookResultsTargetType.parse
    ; target_id = Aws.Util.option_bind (Aws.Xml.member "TargetId" xml) String.parse
    ; hook_results =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "HookResults" xml)
             HookResultSummaries.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("HookResults.member", HookResultSummaries.to_query v.hook_results))
       ; Aws.Util.option_map v.target_id (fun f ->
             Aws.Query.Pair ("TargetId", String.to_query f))
       ; Aws.Util.option_map v.target_type (fun f ->
             Aws.Query.Pair ("TargetType", ListHookResultsTargetType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("HookResults", HookResultSummaries.to_json v.hook_results)
       ; Aws.Util.option_map v.target_id (fun f -> "TargetId", String.to_json f)
       ; Aws.Util.option_map v.target_type (fun f ->
             "TargetType", ListHookResultsTargetType.to_json f)
       ])

let of_json j =
  { target_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetType")
        ListHookResultsTargetType.of_json
  ; target_id = Aws.Util.option_map (Aws.Json.lookup j "TargetId") String.of_json
  ; hook_results =
      HookResultSummaries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "HookResults"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
