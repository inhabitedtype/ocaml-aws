open Aws.BaseTypes

type t =
  { serverless_cache_name : String.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make ?serverless_cache_name ?max_results ?next_token () =
  { serverless_cache_name; max_results; next_token }

let parse xml =
  Some
    { serverless_cache_name =
        Aws.Util.option_bind (Aws.Xml.member "ServerlessCacheName" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.serverless_cache_name (fun f ->
             Aws.Query.Pair ("ServerlessCacheName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.serverless_cache_name (fun f ->
             "ServerlessCacheName", String.to_json f)
       ])

let of_json j =
  { serverless_cache_name =
      Aws.Util.option_map (Aws.Json.lookup j "ServerlessCacheName") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
