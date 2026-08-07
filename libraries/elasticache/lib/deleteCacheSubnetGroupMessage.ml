open Aws.BaseTypes

type t = { cache_subnet_group_name : String.t }

let make ~cache_subnet_group_name () = { cache_subnet_group_name }

let parse xml =
  Some
    { cache_subnet_group_name =
        Aws.Xml.required
          "CacheSubnetGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("CacheSubnetGroupName", String.to_query v.cache_subnet_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CacheSubnetGroupName", String.to_json v.cache_subnet_group_name) ])

let of_json j =
  { cache_subnet_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSubnetGroupName"))
  }
