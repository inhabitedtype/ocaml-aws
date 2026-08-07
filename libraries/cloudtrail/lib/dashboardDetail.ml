open Aws.BaseTypes

type t =
  { dashboard_arn : String.t option
  ; type_ : DashboardType.t option
  }

let make ?dashboard_arn ?type_ () = { dashboard_arn; type_ }

let parse xml =
  Some
    { dashboard_arn =
        Aws.Util.option_bind (Aws.Xml.member "DashboardArn" xml) String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) DashboardType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", DashboardType.to_query f))
       ; Aws.Util.option_map v.dashboard_arn (fun f ->
             Aws.Query.Pair ("DashboardArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_ (fun f -> "Type", DashboardType.to_json f)
       ; Aws.Util.option_map v.dashboard_arn (fun f -> "DashboardArn", String.to_json f)
       ])

let of_json j =
  { dashboard_arn = Aws.Util.option_map (Aws.Json.lookup j "DashboardArn") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") DashboardType.of_json
  }
