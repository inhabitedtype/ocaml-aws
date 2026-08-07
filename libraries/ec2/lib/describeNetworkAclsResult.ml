open Aws.BaseTypes

type t =
  { network_acls : NetworkAclList.t
  ; next_token : String.t option
  }

let make ?(network_acls = []) ?next_token () = { network_acls; next_token }

let parse xml =
  Some
    { network_acls =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "networkAclSet" xml) NetworkAclList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("NetworkAclSet", NetworkAclList.to_query v.network_acls))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("networkAclSet", NetworkAclList.to_json v.network_acls)
       ])

let of_json j =
  { network_acls =
      NetworkAclList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "networkAclSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
