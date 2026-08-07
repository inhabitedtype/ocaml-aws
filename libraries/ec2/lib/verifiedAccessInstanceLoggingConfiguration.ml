open Aws.BaseTypes

type t =
  { verified_access_instance_id : String.t option
  ; access_logs : VerifiedAccessLogs.t option
  }

let make ?verified_access_instance_id ?access_logs () =
  { verified_access_instance_id; access_logs }

let parse xml =
  Some
    { verified_access_instance_id =
        Aws.Util.option_bind (Aws.Xml.member "verifiedAccessInstanceId" xml) String.parse
    ; access_logs =
        Aws.Util.option_bind (Aws.Xml.member "accessLogs" xml) VerifiedAccessLogs.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.access_logs (fun f ->
             Aws.Query.Pair ("AccessLogs", VerifiedAccessLogs.to_query f))
       ; Aws.Util.option_map v.verified_access_instance_id (fun f ->
             Aws.Query.Pair ("VerifiedAccessInstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.access_logs (fun f ->
             "accessLogs", VerifiedAccessLogs.to_json f)
       ; Aws.Util.option_map v.verified_access_instance_id (fun f ->
             "verifiedAccessInstanceId", String.to_json f)
       ])

let of_json j =
  { verified_access_instance_id =
      Aws.Util.option_map (Aws.Json.lookup j "verifiedAccessInstanceId") String.of_json
  ; access_logs =
      Aws.Util.option_map (Aws.Json.lookup j "accessLogs") VerifiedAccessLogs.of_json
  }
