open Aws.BaseTypes

type t =
  { marker : String.t option
  ; cache_security_groups : CacheSecurityGroups.t
  }

let make ?marker ?(cache_security_groups = []) () = { marker; cache_security_groups }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; cache_security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheSecurityGroups" xml)
             CacheSecurityGroups.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CacheSecurityGroups.member"
              , CacheSecurityGroups.to_query v.cache_security_groups ))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CacheSecurityGroups", CacheSecurityGroups.to_json v.cache_security_groups)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; cache_security_groups =
      CacheSecurityGroups.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroups"))
  }
