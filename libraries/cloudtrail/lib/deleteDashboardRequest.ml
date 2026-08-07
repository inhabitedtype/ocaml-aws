open Aws.BaseTypes

type t = { dashboard_id : String.t }

let make ~dashboard_id () = { dashboard_id }

let parse xml =
  Some
    { dashboard_id =
        Aws.Xml.required
          "DashboardId"
          (Aws.Util.option_bind (Aws.Xml.member "DashboardId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("DashboardId", String.to_query v.dashboard_id)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("DashboardId", String.to_json v.dashboard_id) ])

let of_json j =
  { dashboard_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardId"))
  }
