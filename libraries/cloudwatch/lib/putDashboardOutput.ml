type t = { dashboard_validation_messages : DashboardValidationMessages.t }

let make ?(dashboard_validation_messages = []) () = { dashboard_validation_messages }

let parse xml =
  Some
    { dashboard_validation_messages =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DashboardValidationMessages" xml)
             DashboardValidationMessages.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "DashboardValidationMessages.member"
              , DashboardValidationMessages.to_query v.dashboard_validation_messages ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "DashboardValidationMessages"
           , DashboardValidationMessages.to_json v.dashboard_validation_messages )
       ])

let of_json j =
  { dashboard_validation_messages =
      DashboardValidationMessages.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardValidationMessages"))
  }
