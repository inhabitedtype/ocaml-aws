open Aws.BaseTypes

type t =
  { byoip_cidrs : ByoipCidrSet.t
  ; next_token : String.t option
  }

let make ?(byoip_cidrs = []) ?next_token () = { byoip_cidrs; next_token }

let parse xml =
  Some
    { byoip_cidrs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "byoipCidrSet" xml) ByoipCidrSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("ByoipCidrSet", ByoipCidrSet.to_query v.byoip_cidrs))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("byoipCidrSet", ByoipCidrSet.to_json v.byoip_cidrs)
       ])

let of_json j =
  { byoip_cidrs =
      ByoipCidrSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "byoipCidrSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
