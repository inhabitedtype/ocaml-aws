open Aws.BaseTypes

type t =
  { policy_name : String.t option
  ; cookie_expiration_period : Long.t option
  }

let make ?policy_name ?cookie_expiration_period () =
  { policy_name; cookie_expiration_period }

let parse xml =
  Some
    { policy_name = Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse
    ; cookie_expiration_period =
        Aws.Util.option_bind (Aws.Xml.member "CookieExpirationPeriod" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cookie_expiration_period (fun f ->
             Aws.Query.Pair ("CookieExpirationPeriod", Long.to_query f))
       ; Aws.Util.option_map v.policy_name (fun f ->
             Aws.Query.Pair ("PolicyName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cookie_expiration_period (fun f ->
             "CookieExpirationPeriod", Long.to_json f)
       ; Aws.Util.option_map v.policy_name (fun f -> "PolicyName", String.to_json f)
       ])

let of_json j =
  { policy_name = Aws.Util.option_map (Aws.Json.lookup j "PolicyName") String.of_json
  ; cookie_expiration_period =
      Aws.Util.option_map (Aws.Json.lookup j "CookieExpirationPeriod") Long.of_json
  }
