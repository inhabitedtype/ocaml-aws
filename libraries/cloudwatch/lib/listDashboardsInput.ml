open Aws.BaseTypes

type t =
  { dashboard_name_prefix : String.t option
  ; next_token : String.t option
  }

let make ?dashboard_name_prefix ?next_token () = { dashboard_name_prefix; next_token }

let parse xml =
  Some
    { dashboard_name_prefix =
        Aws.Util.option_bind (Aws.Xml.member "DashboardNamePrefix" xml) String.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.dashboard_name_prefix (fun f ->
             Aws.Query.Pair ("DashboardNamePrefix", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.dashboard_name_prefix (fun f ->
             "DashboardNamePrefix", String.to_json f)
       ])

let of_json j =
  { dashboard_name_prefix =
      Aws.Util.option_map (Aws.Json.lookup j "DashboardNamePrefix") String.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
