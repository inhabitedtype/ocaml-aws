open Aws.BaseTypes

type t = { cache_parameter_group_name : String.t }

let make ~cache_parameter_group_name () = { cache_parameter_group_name }

let parse xml =
  Some
    { cache_parameter_group_name =
        Aws.Xml.required
          "CacheParameterGroupName"
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheParameterGroupName" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("CacheParameterGroupName", String.to_query v.cache_parameter_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CacheParameterGroupName", String.to_json v.cache_parameter_group_name) ])

let of_json j =
  { cache_parameter_group_name =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheParameterGroupName"))
  }
