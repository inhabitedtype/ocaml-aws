open Aws.BaseTypes

type t =
  { load_balancer_name : String.t
  ; policy_name : String.t
  ; cookie_expiration_period : Long.t option
  }

let make ~load_balancer_name ~policy_name ?cookie_expiration_period () =
  { load_balancer_name; policy_name; cookie_expiration_period }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Xml.required
          "LoadBalancerName"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
    ; policy_name =
        Aws.Xml.required
          "PolicyName"
          (Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse)
    ; cookie_expiration_period =
        Aws.Util.option_bind (Aws.Xml.member "CookieExpirationPeriod" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cookie_expiration_period (fun f ->
             Aws.Query.Pair ("CookieExpirationPeriod", Long.to_query f))
       ; Some (Aws.Query.Pair ("PolicyName", String.to_query v.policy_name))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cookie_expiration_period (fun f ->
             "CookieExpirationPeriod", Long.to_json f)
       ; Some ("PolicyName", String.to_json v.policy_name)
       ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
       ])

let of_json j =
  { load_balancer_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
  ; policy_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyName"))
  ; cookie_expiration_period =
      Aws.Util.option_map (Aws.Json.lookup j "CookieExpirationPeriod") Long.of_json
  }
