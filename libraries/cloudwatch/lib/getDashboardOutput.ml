open Aws.BaseTypes

type t =
  { dashboard_arn : String.t option
  ; dashboard_body : String.t option
  ; dashboard_name : String.t option
  }

let make ?dashboard_arn ?dashboard_body ?dashboard_name () =
  { dashboard_arn; dashboard_body; dashboard_name }

let parse xml =
  Some
    { dashboard_arn =
        Aws.Util.option_bind (Aws.Xml.member "DashboardArn" xml) String.parse
    ; dashboard_body =
        Aws.Util.option_bind (Aws.Xml.member "DashboardBody" xml) String.parse
    ; dashboard_name =
        Aws.Util.option_bind (Aws.Xml.member "DashboardName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dashboard_name (fun f ->
             Aws.Query.Pair ("DashboardName", String.to_query f))
       ; Aws.Util.option_map v.dashboard_body (fun f ->
             Aws.Query.Pair ("DashboardBody", String.to_query f))
       ; Aws.Util.option_map v.dashboard_arn (fun f ->
             Aws.Query.Pair ("DashboardArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dashboard_name (fun f -> "DashboardName", String.to_json f)
       ; Aws.Util.option_map v.dashboard_body (fun f -> "DashboardBody", String.to_json f)
       ; Aws.Util.option_map v.dashboard_arn (fun f -> "DashboardArn", String.to_json f)
       ])

let of_json j =
  { dashboard_arn = Aws.Util.option_map (Aws.Json.lookup j "DashboardArn") String.of_json
  ; dashboard_body =
      Aws.Util.option_map (Aws.Json.lookup j "DashboardBody") String.of_json
  ; dashboard_name =
      Aws.Util.option_map (Aws.Json.lookup j "DashboardName") String.of_json
  }
