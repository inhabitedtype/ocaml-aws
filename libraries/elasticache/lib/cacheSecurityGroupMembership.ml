open Aws.BaseTypes

type t =
  { cache_security_group_name : String.t option
  ; status : String.t option
  }

let make ?cache_security_group_name ?status () = { cache_security_group_name; status }

let parse xml =
  Some
    { cache_security_group_name =
        Aws.Util.option_bind (Aws.Xml.member "CacheSecurityGroupName" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.cache_security_group_name (fun f ->
             Aws.Query.Pair ("CacheSecurityGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.cache_security_group_name (fun f ->
             "CacheSecurityGroupName", String.to_json f)
       ])

let of_json j =
  { cache_security_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "CacheSecurityGroupName") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  }
