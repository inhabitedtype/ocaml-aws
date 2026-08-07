open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; public_ips : ValueStringList.t
  ; next_token : String.t option
  ; filters : FilterList.t
  ; max_results : Integer.t option
  }

let make ?dry_run ?(public_ips = []) ?next_token ?(filters = []) ?max_results () =
  { dry_run; public_ips; next_token; filters; max_results }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; public_ips =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "publicIp" xml) ValueStringList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "filter" xml) FilterList.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "maxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("PublicIp", ValueStringList.to_query v.public_ips))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "maxResults", Integer.to_json f)
       ; Some ("filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("publicIp", ValueStringList.to_json v.public_ips)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; public_ips =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "publicIp"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "filter"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxResults") Integer.of_json
  }
