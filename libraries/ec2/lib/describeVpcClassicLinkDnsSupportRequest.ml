open Aws.BaseTypes

type t =
  { vpc_ids : VpcClassicLinkIdList.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make ?(vpc_ids = []) ?max_results ?next_token () =
  { vpc_ids; max_results; next_token }

let parse xml =
  Some
    { vpc_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "VpcIds" xml) VpcClassicLinkIdList.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "maxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some (Aws.Query.Pair ("VpcIds", VpcClassicLinkIdList.to_query v.vpc_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "maxResults", Integer.to_json f)
       ; Some ("VpcIds", VpcClassicLinkIdList.to_json v.vpc_ids)
       ])

let of_json j =
  { vpc_ids =
      VpcClassicLinkIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcIds"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
