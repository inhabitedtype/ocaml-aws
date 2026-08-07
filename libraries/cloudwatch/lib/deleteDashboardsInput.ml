type t = { dashboard_names : DashboardNames.t }

let make ~dashboard_names () = { dashboard_names }

let parse xml =
  Some
    { dashboard_names =
        Aws.Xml.required
          "DashboardNames"
          (Aws.Util.option_bind
             (Aws.Xml.member "DashboardNames" xml)
             DashboardNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("DashboardNames.member", DashboardNames.to_query v.dashboard_names))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DashboardNames", DashboardNames.to_json v.dashboard_names) ])

let of_json j =
  { dashboard_names =
      DashboardNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardNames"))
  }
