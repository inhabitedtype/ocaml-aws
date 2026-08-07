open Aws.BaseTypes

type t =
  { filters : FilterList.t
  ; host_ids : RequestHostIdList.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make ?(filters = []) ?(host_ids = []) ?max_results ?next_token () =
  { filters; host_ids; max_results; next_token }

let parse xml =
  Some
    { filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; host_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "HostId" xml) RequestHostIdList.parse)
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
       ; Some (Aws.Query.Pair ("HostId", RequestHostIdList.to_query v.host_ids))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("HostId", RequestHostIdList.to_json v.host_ids)
       ; Some ("Filter", FilterList.to_json v.filters)
       ])

let of_json j =
  { filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; host_ids =
      RequestHostIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostId"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
