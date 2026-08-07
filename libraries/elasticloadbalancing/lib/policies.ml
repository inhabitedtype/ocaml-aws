type t =
  { app_cookie_stickiness_policies : AppCookieStickinessPolicies.t
  ; l_b_cookie_stickiness_policies : LBCookieStickinessPolicies.t
  ; other_policies : PolicyNames.t
  }

let make
    ?(app_cookie_stickiness_policies = [])
    ?(l_b_cookie_stickiness_policies = [])
    ?(other_policies = [])
    () =
  { app_cookie_stickiness_policies; l_b_cookie_stickiness_policies; other_policies }

let parse xml =
  Some
    { app_cookie_stickiness_policies =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AppCookieStickinessPolicies" xml)
             AppCookieStickinessPolicies.parse)
    ; l_b_cookie_stickiness_policies =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LBCookieStickinessPolicies" xml)
             LBCookieStickinessPolicies.parse)
    ; other_policies =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "OtherPolicies" xml) PolicyNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("OtherPolicies.member", PolicyNames.to_query v.other_policies))
       ; Some
           (Aws.Query.Pair
              ( "LBCookieStickinessPolicies.member"
              , LBCookieStickinessPolicies.to_query v.l_b_cookie_stickiness_policies ))
       ; Some
           (Aws.Query.Pair
              ( "AppCookieStickinessPolicies.member"
              , AppCookieStickinessPolicies.to_query v.app_cookie_stickiness_policies ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("OtherPolicies", PolicyNames.to_json v.other_policies)
       ; Some
           ( "LBCookieStickinessPolicies"
           , LBCookieStickinessPolicies.to_json v.l_b_cookie_stickiness_policies )
       ; Some
           ( "AppCookieStickinessPolicies"
           , AppCookieStickinessPolicies.to_json v.app_cookie_stickiness_policies )
       ])

let of_json j =
  { app_cookie_stickiness_policies =
      AppCookieStickinessPolicies.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AppCookieStickinessPolicies"))
  ; l_b_cookie_stickiness_policies =
      LBCookieStickinessPolicies.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LBCookieStickinessPolicies"))
  ; other_policies =
      PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OtherPolicies"))
  }
