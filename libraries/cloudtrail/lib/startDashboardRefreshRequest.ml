open Aws.BaseTypes

type t =
  { dashboard_id : String.t
  ; query_parameter_values : QueryParameterValues.t option
  }

let make ~dashboard_id ?query_parameter_values () =
  { dashboard_id; query_parameter_values }

let parse xml =
  Some
    { dashboard_id =
        Aws.Xml.required
          "DashboardId"
          (Aws.Util.option_bind (Aws.Xml.member "DashboardId" xml) String.parse)
    ; query_parameter_values =
        Aws.Util.option_bind
          (Aws.Xml.member "QueryParameterValues" xml)
          QueryParameterValues.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.query_parameter_values (fun f ->
             Aws.Query.Pair ("QueryParameterValues", QueryParameterValues.to_query f))
       ; Some (Aws.Query.Pair ("DashboardId", String.to_query v.dashboard_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.query_parameter_values (fun f ->
             "QueryParameterValues", QueryParameterValues.to_json f)
       ; Some ("DashboardId", String.to_json v.dashboard_id)
       ])

let of_json j =
  { dashboard_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardId"))
  ; query_parameter_values =
      Aws.Util.option_map
        (Aws.Json.lookup j "QueryParameterValues")
        QueryParameterValues.of_json
  }
