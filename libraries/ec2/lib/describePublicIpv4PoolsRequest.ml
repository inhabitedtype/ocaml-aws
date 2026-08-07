open Aws.BaseTypes

type t =
  { pool_ids : PublicIpv4PoolIdStringList.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; filters : FilterList.t
  }

let make ?(pool_ids = []) ?next_token ?max_results ?(filters = []) () =
  { pool_ids; next_token; max_results; filters }

let parse xml =
  Some
    { pool_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PoolId" xml)
             PublicIpv4PoolIdStringList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("PoolId", PublicIpv4PoolIdStringList.to_query v.pool_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("PoolId", PublicIpv4PoolIdStringList.to_json v.pool_ids)
       ])

let of_json j =
  { pool_ids =
      PublicIpv4PoolIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PoolId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
