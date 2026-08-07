open Aws.BaseTypes

type t =
  { marker : String.t option
  ; cache_subnet_groups : CacheSubnetGroups.t
  }

let make ?marker ?(cache_subnet_groups = []) () = { marker; cache_subnet_groups }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; cache_subnet_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheSubnetGroups" xml)
             CacheSubnetGroups.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CacheSubnetGroups.member"
              , CacheSubnetGroups.to_query v.cache_subnet_groups ))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CacheSubnetGroups", CacheSubnetGroups.to_json v.cache_subnet_groups)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; cache_subnet_groups =
      CacheSubnetGroups.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSubnetGroups"))
  }
