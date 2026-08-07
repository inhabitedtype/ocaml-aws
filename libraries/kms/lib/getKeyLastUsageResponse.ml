open Aws.BaseTypes

type t =
  { key_id : String.t option
  ; key_last_usage : KeyLastUsageData.t option
  ; tracking_start_date : DateTime.t option
  ; key_creation_date : DateTime.t option
  }

let make ?key_id ?key_last_usage ?tracking_start_date ?key_creation_date () =
  { key_id; key_last_usage; tracking_start_date; key_creation_date }

let parse xml =
  Some
    { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; key_last_usage =
        Aws.Util.option_bind (Aws.Xml.member "KeyLastUsage" xml) KeyLastUsageData.parse
    ; tracking_start_date =
        Aws.Util.option_bind (Aws.Xml.member "TrackingStartDate" xml) DateTime.parse
    ; key_creation_date =
        Aws.Util.option_bind (Aws.Xml.member "KeyCreationDate" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_creation_date (fun f ->
             Aws.Query.Pair ("KeyCreationDate", DateTime.to_query f))
       ; Aws.Util.option_map v.tracking_start_date (fun f ->
             Aws.Query.Pair ("TrackingStartDate", DateTime.to_query f))
       ; Aws.Util.option_map v.key_last_usage (fun f ->
             Aws.Query.Pair ("KeyLastUsage", KeyLastUsageData.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_creation_date (fun f ->
             "KeyCreationDate", DateTime.to_json f)
       ; Aws.Util.option_map v.tracking_start_date (fun f ->
             "TrackingStartDate", DateTime.to_json f)
       ; Aws.Util.option_map v.key_last_usage (fun f ->
             "KeyLastUsage", KeyLastUsageData.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ])

let of_json j =
  { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; key_last_usage =
      Aws.Util.option_map (Aws.Json.lookup j "KeyLastUsage") KeyLastUsageData.of_json
  ; tracking_start_date =
      Aws.Util.option_map (Aws.Json.lookup j "TrackingStartDate") DateTime.of_json
  ; key_creation_date =
      Aws.Util.option_map (Aws.Json.lookup j "KeyCreationDate") DateTime.of_json
  }
