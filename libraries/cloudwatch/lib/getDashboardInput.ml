open Aws.BaseTypes

type t = { dashboard_name : String.t }

let make ~dashboard_name () = { dashboard_name }

let parse xml =
  Some
    { dashboard_name =
        Aws.Xml.required
          "DashboardName"
          (Aws.Util.option_bind (Aws.Xml.member "DashboardName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("DashboardName", String.to_query v.dashboard_name)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("DashboardName", String.to_json v.dashboard_name) ])

let of_json j =
  { dashboard_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardName"))
  }
