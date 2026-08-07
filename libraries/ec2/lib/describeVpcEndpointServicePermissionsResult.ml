open Aws.BaseTypes

type t =
  { allowed_principals : AllowedPrincipalSet.t
  ; next_token : String.t option
  }

let make ?(allowed_principals = []) ?next_token () = { allowed_principals; next_token }

let parse xml =
  Some
    { allowed_principals =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "allowedPrincipals" xml)
             AllowedPrincipalSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AllowedPrincipals", AllowedPrincipalSet.to_query v.allowed_principals))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("allowedPrincipals", AllowedPrincipalSet.to_json v.allowed_principals)
       ])

let of_json j =
  { allowed_principals =
      AllowedPrincipalSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "allowedPrincipals"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
