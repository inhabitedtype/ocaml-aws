open Aws.BaseTypes

type t =
  { value : Blob.t option
  ; validity_start_time : DateTime.t option
  ; validity_end_time : DateTime.t option
  ; fingerprint : String.t option
  }

let make ?value ?validity_start_time ?validity_end_time ?fingerprint () =
  { value; validity_start_time; validity_end_time; fingerprint }

let parse xml =
  Some
    { value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) Blob.parse
    ; validity_start_time =
        Aws.Util.option_bind (Aws.Xml.member "ValidityStartTime" xml) DateTime.parse
    ; validity_end_time =
        Aws.Util.option_bind (Aws.Xml.member "ValidityEndTime" xml) DateTime.parse
    ; fingerprint = Aws.Util.option_bind (Aws.Xml.member "Fingerprint" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fingerprint (fun f ->
             Aws.Query.Pair ("Fingerprint", String.to_query f))
       ; Aws.Util.option_map v.validity_end_time (fun f ->
             Aws.Query.Pair ("ValidityEndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.validity_start_time (fun f ->
             Aws.Query.Pair ("ValidityStartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.value (fun f -> Aws.Query.Pair ("Value", Blob.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fingerprint (fun f -> "Fingerprint", String.to_json f)
       ; Aws.Util.option_map v.validity_end_time (fun f ->
             "ValidityEndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.validity_start_time (fun f ->
             "ValidityStartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.value (fun f -> "Value", Blob.to_json f)
       ])

let of_json j =
  { value = Aws.Util.option_map (Aws.Json.lookup j "Value") Blob.of_json
  ; validity_start_time =
      Aws.Util.option_map (Aws.Json.lookup j "ValidityStartTime") DateTime.of_json
  ; validity_end_time =
      Aws.Util.option_map (Aws.Json.lookup j "ValidityEndTime") DateTime.of_json
  ; fingerprint = Aws.Util.option_map (Aws.Json.lookup j "Fingerprint") String.of_json
  }
