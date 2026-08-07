open Aws.BaseTypes

type t =
  { dashboard_name : String.t option
  ; dashboard_arn : String.t option
  ; last_modified : DateTime.t option
  ; size : Long.t option
  }

let make ?dashboard_name ?dashboard_arn ?last_modified ?size () =
  { dashboard_name; dashboard_arn; last_modified; size }

let parse xml =
  Some
    { dashboard_name =
        Aws.Util.option_bind (Aws.Xml.member "DashboardName" xml) String.parse
    ; dashboard_arn =
        Aws.Util.option_bind (Aws.Xml.member "DashboardArn" xml) String.parse
    ; last_modified =
        Aws.Util.option_bind (Aws.Xml.member "LastModified" xml) DateTime.parse
    ; size = Aws.Util.option_bind (Aws.Xml.member "Size" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.size (fun f -> Aws.Query.Pair ("Size", Long.to_query f))
       ; Aws.Util.option_map v.last_modified (fun f ->
             Aws.Query.Pair ("LastModified", DateTime.to_query f))
       ; Aws.Util.option_map v.dashboard_arn (fun f ->
             Aws.Query.Pair ("DashboardArn", String.to_query f))
       ; Aws.Util.option_map v.dashboard_name (fun f ->
             Aws.Query.Pair ("DashboardName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.size (fun f -> "Size", Long.to_json f)
       ; Aws.Util.option_map v.last_modified (fun f -> "LastModified", DateTime.to_json f)
       ; Aws.Util.option_map v.dashboard_arn (fun f -> "DashboardArn", String.to_json f)
       ; Aws.Util.option_map v.dashboard_name (fun f -> "DashboardName", String.to_json f)
       ])

let of_json j =
  { dashboard_name =
      Aws.Util.option_map (Aws.Json.lookup j "DashboardName") String.of_json
  ; dashboard_arn = Aws.Util.option_map (Aws.Json.lookup j "DashboardArn") String.of_json
  ; last_modified =
      Aws.Util.option_map (Aws.Json.lookup j "LastModified") DateTime.of_json
  ; size = Aws.Util.option_map (Aws.Json.lookup j "Size") Long.of_json
  }
