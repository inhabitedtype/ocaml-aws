type t =
  { enable_log_types : LogTypeList.t
  ; disable_log_types : LogTypeList.t
  }

let make ?(enable_log_types = []) ?(disable_log_types = []) () =
  { enable_log_types; disable_log_types }

let parse xml =
  Some
    { enable_log_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "EnableLogTypes" xml) LogTypeList.parse)
    ; disable_log_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "DisableLogTypes" xml) LogTypeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("DisableLogTypes.member", LogTypeList.to_query v.disable_log_types))
       ; Some
           (Aws.Query.Pair
              ("EnableLogTypes.member", LogTypeList.to_query v.enable_log_types))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DisableLogTypes", LogTypeList.to_json v.disable_log_types)
       ; Some ("EnableLogTypes", LogTypeList.to_json v.enable_log_types)
       ])

let of_json j =
  { enable_log_types =
      LogTypeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EnableLogTypes"))
  ; disable_log_types =
      LogTypeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DisableLogTypes"))
  }
