open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; stale_security_group_set : StaleSecurityGroupSet.t
  }

let make ?next_token ?(stale_security_group_set = []) () =
  { next_token; stale_security_group_set }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; stale_security_group_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "staleSecurityGroupSet" xml)
             StaleSecurityGroupSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "StaleSecurityGroupSet"
              , StaleSecurityGroupSet.to_query v.stale_security_group_set ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "staleSecurityGroupSet"
           , StaleSecurityGroupSet.to_json v.stale_security_group_set )
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; stale_security_group_set =
      StaleSecurityGroupSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "staleSecurityGroupSet"))
  }
