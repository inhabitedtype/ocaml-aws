open Aws.BaseTypes

type t = { cache_security_group_name : String.t }

let make ~cache_security_group_name () = { cache_security_group_name }

let parse xml =
  Some
    { cache_security_group_name =
        Aws.Xml.required
          "CacheSecurityGroupName"
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheSecurityGroupName" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("CacheSecurityGroupName", String.to_query v.cache_security_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CacheSecurityGroupName", String.to_json v.cache_security_group_name) ])

let of_json j =
  { cache_security_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupName"))
  }
