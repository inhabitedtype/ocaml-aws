open Aws.BaseTypes

type t =
  { accepted_route_count : Integer.t option
  ; last_status_change : DateTime.t option
  ; outside_ip_address : String.t option
  ; status : TelemetryStatus.t option
  ; status_message : String.t option
  ; certificate_arn : String.t option
  }

let make
    ?accepted_route_count
    ?last_status_change
    ?outside_ip_address
    ?status
    ?status_message
    ?certificate_arn
    () =
  { accepted_route_count
  ; last_status_change
  ; outside_ip_address
  ; status
  ; status_message
  ; certificate_arn
  }

let parse xml =
  Some
    { accepted_route_count =
        Aws.Util.option_bind (Aws.Xml.member "acceptedRouteCount" xml) Integer.parse
    ; last_status_change =
        Aws.Util.option_bind (Aws.Xml.member "lastStatusChange" xml) DateTime.parse
    ; outside_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "outsideIpAddress" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) TelemetryStatus.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; certificate_arn =
        Aws.Util.option_bind (Aws.Xml.member "certificateArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.certificate_arn (fun f ->
             Aws.Query.Pair ("CertificateArn", String.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", TelemetryStatus.to_query f))
       ; Aws.Util.option_map v.outside_ip_address (fun f ->
             Aws.Query.Pair ("OutsideIpAddress", String.to_query f))
       ; Aws.Util.option_map v.last_status_change (fun f ->
             Aws.Query.Pair ("LastStatusChange", DateTime.to_query f))
       ; Aws.Util.option_map v.accepted_route_count (fun f ->
             Aws.Query.Pair ("AcceptedRouteCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.certificate_arn (fun f ->
             "certificateArn", String.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", TelemetryStatus.to_json f)
       ; Aws.Util.option_map v.outside_ip_address (fun f ->
             "outsideIpAddress", String.to_json f)
       ; Aws.Util.option_map v.last_status_change (fun f ->
             "lastStatusChange", DateTime.to_json f)
       ; Aws.Util.option_map v.accepted_route_count (fun f ->
             "acceptedRouteCount", Integer.to_json f)
       ])

let of_json j =
  { accepted_route_count =
      Aws.Util.option_map (Aws.Json.lookup j "acceptedRouteCount") Integer.of_json
  ; last_status_change =
      Aws.Util.option_map (Aws.Json.lookup j "lastStatusChange") DateTime.of_json
  ; outside_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "outsideIpAddress") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") TelemetryStatus.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; certificate_arn =
      Aws.Util.option_map (Aws.Json.lookup j "certificateArn") String.of_json
  }
