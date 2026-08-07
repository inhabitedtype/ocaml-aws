open Aws.BaseTypes

type t =
  { policy_name : String.t option
  ; cookie_name : String.t option
  }

let make ?policy_name ?cookie_name () = { policy_name; cookie_name }

let parse xml =
  Some
    { policy_name = Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse
    ; cookie_name = Aws.Util.option_bind (Aws.Xml.member "CookieName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cookie_name (fun f ->
             Aws.Query.Pair ("CookieName", String.to_query f))
       ; Aws.Util.option_map v.policy_name (fun f ->
             Aws.Query.Pair ("PolicyName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cookie_name (fun f -> "CookieName", String.to_json f)
       ; Aws.Util.option_map v.policy_name (fun f -> "PolicyName", String.to_json f)
       ])

let of_json j =
  { policy_name = Aws.Util.option_map (Aws.Json.lookup j "PolicyName") String.of_json
  ; cookie_name = Aws.Util.option_map (Aws.Json.lookup j "CookieName") String.of_json
  }
