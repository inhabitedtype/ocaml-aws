open Aws.BaseTypes

type t =
  { region : HealthCheckRegion.t option
  ; i_p_address : String.t option
  ; status_report : StatusReport.t option
  }

let make ?region ?i_p_address ?status_report () = { region; i_p_address; status_report }

let parse xml =
  Some
    { region = Aws.Util.option_bind (Aws.Xml.member "Region" xml) HealthCheckRegion.parse
    ; i_p_address = Aws.Util.option_bind (Aws.Xml.member "IPAddress" xml) String.parse
    ; status_report =
        Aws.Util.option_bind (Aws.Xml.member "StatusReport" xml) StatusReport.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status_report (fun f ->
             Aws.Query.Pair ("StatusReport", StatusReport.to_query f))
       ; Aws.Util.option_map v.i_p_address (fun f ->
             Aws.Query.Pair ("IPAddress", String.to_query f))
       ; Aws.Util.option_map v.region (fun f ->
             Aws.Query.Pair ("Region", HealthCheckRegion.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status_report (fun f ->
             "StatusReport", StatusReport.to_json f)
       ; Aws.Util.option_map v.i_p_address (fun f -> "IPAddress", String.to_json f)
       ; Aws.Util.option_map v.region (fun f -> "Region", HealthCheckRegion.to_json f)
       ])

let of_json j =
  { region = Aws.Util.option_map (Aws.Json.lookup j "Region") HealthCheckRegion.of_json
  ; i_p_address = Aws.Util.option_map (Aws.Json.lookup j "IPAddress") String.of_json
  ; status_report =
      Aws.Util.option_map (Aws.Json.lookup j "StatusReport") StatusReport.of_json
  }
