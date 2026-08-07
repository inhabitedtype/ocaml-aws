open Aws.BaseTypes

type t =
  { host_ids : RequestHostIdList.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; filter : FilterList.t
  }

let make ?(host_ids = []) ?next_token ?max_results ?(filter = []) () =
  { host_ids; next_token; max_results; filter }

let parse xml =
  Some
    { host_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "hostId" xml) RequestHostIdList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "maxResults" xml) Integer.parse
    ; filter =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filter))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("HostId", RequestHostIdList.to_query v.host_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("filter", FilterList.to_json v.filter)
       ; Aws.Util.option_map v.max_results (fun f -> "maxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("hostId", RequestHostIdList.to_json v.host_ids)
       ])

let of_json j =
  { host_ids =
      RequestHostIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "hostId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxResults") Integer.of_json
  ; filter = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "filter"))
  }
