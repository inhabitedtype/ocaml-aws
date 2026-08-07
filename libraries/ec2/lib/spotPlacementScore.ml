open Aws.BaseTypes

type t =
  { region : String.t option
  ; availability_zone_id : String.t option
  ; score : Integer.t option
  }

let make ?region ?availability_zone_id ?score () = { region; availability_zone_id; score }

let parse xml =
  Some
    { region = Aws.Util.option_bind (Aws.Xml.member "region" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; score = Aws.Util.option_bind (Aws.Xml.member "score" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.score (fun f ->
             Aws.Query.Pair ("Score", Integer.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.region (fun f ->
             Aws.Query.Pair ("Region", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.score (fun f -> "score", Integer.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.region (fun f -> "region", String.to_json f)
       ])

let of_json j =
  { region = Aws.Util.option_map (Aws.Json.lookup j "region") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; score = Aws.Util.option_map (Aws.Json.lookup j "score") Integer.of_json
  }
