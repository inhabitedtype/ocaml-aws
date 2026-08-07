open Aws.BaseTypes

type t =
  { coip_pools : CoipPoolSet.t
  ; next_token : String.t option
  }

let make ?(coip_pools = []) ?next_token () = { coip_pools; next_token }

let parse xml =
  Some
    { coip_pools =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "coipPoolSet" xml) CoipPoolSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("CoipPoolSet", CoipPoolSet.to_query v.coip_pools))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("coipPoolSet", CoipPoolSet.to_json v.coip_pools)
       ])

let of_json j =
  { coip_pools =
      CoipPoolSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "coipPoolSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
