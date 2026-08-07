type t =
  { log_types_to_enable : LogTypeList.t
  ; log_types_to_disable : LogTypeList.t
  }

let make ?(log_types_to_enable = []) ?(log_types_to_disable = []) () =
  { log_types_to_enable; log_types_to_disable }

let parse xml =
  Some
    { log_types_to_enable =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "LogTypesToEnable" xml) LogTypeList.parse)
    ; log_types_to_disable =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LogTypesToDisable" xml)
             LogTypeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("LogTypesToDisable.member", LogTypeList.to_query v.log_types_to_disable))
       ; Some
           (Aws.Query.Pair
              ("LogTypesToEnable.member", LogTypeList.to_query v.log_types_to_enable))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LogTypesToDisable", LogTypeList.to_json v.log_types_to_disable)
       ; Some ("LogTypesToEnable", LogTypeList.to_json v.log_types_to_enable)
       ])

let of_json j =
  { log_types_to_enable =
      LogTypeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogTypesToEnable"))
  ; log_types_to_disable =
      LogTypeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogTypesToDisable"))
  }
