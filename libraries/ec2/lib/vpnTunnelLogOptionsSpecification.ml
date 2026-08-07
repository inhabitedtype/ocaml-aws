type t = { cloud_watch_log_options : CloudWatchLogOptionsSpecification.t option }

let make ?cloud_watch_log_options () = { cloud_watch_log_options }

let parse xml =
  Some
    { cloud_watch_log_options =
        Aws.Util.option_bind
          (Aws.Xml.member "CloudWatchLogOptions" xml)
          CloudWatchLogOptionsSpecification.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cloud_watch_log_options (fun f ->
             Aws.Query.Pair
               ("CloudWatchLogOptions", CloudWatchLogOptionsSpecification.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cloud_watch_log_options (fun f ->
             "CloudWatchLogOptions", CloudWatchLogOptionsSpecification.to_json f)
       ])

let of_json j =
  { cloud_watch_log_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "CloudWatchLogOptions")
        CloudWatchLogOptionsSpecification.of_json
  }
