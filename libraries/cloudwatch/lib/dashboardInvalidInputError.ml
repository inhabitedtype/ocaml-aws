open Aws.BaseTypes

type t =
  { message : String.t option
  ; dashboard_validation_messages : DashboardValidationMessages.t
  }

let make ?message ?(dashboard_validation_messages = []) () =
  { message; dashboard_validation_messages }

let parse xml =
  Some
    { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse
    ; dashboard_validation_messages =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "dashboardValidationMessages" xml)
             DashboardValidationMessages.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "dashboardValidationMessages.member"
              , DashboardValidationMessages.to_query v.dashboard_validation_messages ))
       ; Aws.Util.option_map v.message (fun f ->
             Aws.Query.Pair ("message", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "dashboardValidationMessages"
           , DashboardValidationMessages.to_json v.dashboard_validation_messages )
       ; Aws.Util.option_map v.message (fun f -> "message", String.to_json f)
       ])

let of_json j =
  { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json
  ; dashboard_validation_messages =
      DashboardValidationMessages.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "dashboardValidationMessages"))
  }
