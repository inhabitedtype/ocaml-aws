open Aws.BaseTypes

type t =
  { cache_parameter_group_name : String.t
  ; parameter_name_values : ParameterNameValueList.t
  }

let make ~cache_parameter_group_name ~parameter_name_values () =
  { cache_parameter_group_name; parameter_name_values }

let parse xml =
  Some
    { cache_parameter_group_name =
        Aws.Xml.required
          "CacheParameterGroupName"
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheParameterGroupName" xml)
             String.parse)
    ; parameter_name_values =
        Aws.Xml.required
          "ParameterNameValues"
          (Aws.Util.option_bind
             (Aws.Xml.member "ParameterNameValues" xml)
             ParameterNameValueList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ParameterNameValues.member"
              , ParameterNameValueList.to_query v.parameter_name_values ))
       ; Some
           (Aws.Query.Pair
              ("CacheParameterGroupName", String.to_query v.cache_parameter_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("ParameterNameValues", ParameterNameValueList.to_json v.parameter_name_values)
       ; Some ("CacheParameterGroupName", String.to_json v.cache_parameter_group_name)
       ])

let of_json j =
  { cache_parameter_group_name =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheParameterGroupName"))
  ; parameter_name_values =
      ParameterNameValueList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ParameterNameValues"))
  }
