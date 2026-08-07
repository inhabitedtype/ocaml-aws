open Aws.BaseTypes

type t =
  { queue_name_prefix : String.t option
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make ?queue_name_prefix ?next_token ?max_results () =
  { queue_name_prefix; next_token; max_results }

let parse xml =
  Some
    { queue_name_prefix =
        Aws.Util.option_bind (Aws.Xml.member "QueueNamePrefix" xml) String.parse
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
       ; Aws.Util.option_map v.queue_name_prefix (fun f ->
             Aws.Query.Pair ("QueueNamePrefix", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.queue_name_prefix (fun f ->
             "QueueNamePrefix", String.to_json f)
       ])

let of_json j =
  { queue_name_prefix =
      Aws.Util.option_map (Aws.Json.lookup j "QueueNamePrefix") String.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
