open Aws.BaseTypes

type t =
  { a_w_s_region : String.t option
  ; local_zone_group : String.t option
  ; coordinates : Coordinates.t option
  ; bias : Integer.t option
  }

let make ?a_w_s_region ?local_zone_group ?coordinates ?bias () =
  { a_w_s_region; local_zone_group; coordinates; bias }

let parse xml =
  Some
    { a_w_s_region = Aws.Util.option_bind (Aws.Xml.member "AWSRegion" xml) String.parse
    ; local_zone_group =
        Aws.Util.option_bind (Aws.Xml.member "LocalZoneGroup" xml) String.parse
    ; coordinates =
        Aws.Util.option_bind (Aws.Xml.member "Coordinates" xml) Coordinates.parse
    ; bias = Aws.Util.option_bind (Aws.Xml.member "Bias" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bias (fun f -> Aws.Query.Pair ("Bias", Integer.to_query f))
       ; Aws.Util.option_map v.coordinates (fun f ->
             Aws.Query.Pair ("Coordinates", Coordinates.to_query f))
       ; Aws.Util.option_map v.local_zone_group (fun f ->
             Aws.Query.Pair ("LocalZoneGroup", String.to_query f))
       ; Aws.Util.option_map v.a_w_s_region (fun f ->
             Aws.Query.Pair ("AWSRegion", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bias (fun f -> "Bias", Integer.to_json f)
       ; Aws.Util.option_map v.coordinates (fun f -> "Coordinates", Coordinates.to_json f)
       ; Aws.Util.option_map v.local_zone_group (fun f ->
             "LocalZoneGroup", String.to_json f)
       ; Aws.Util.option_map v.a_w_s_region (fun f -> "AWSRegion", String.to_json f)
       ])

let of_json j =
  { a_w_s_region = Aws.Util.option_map (Aws.Json.lookup j "AWSRegion") String.of_json
  ; local_zone_group =
      Aws.Util.option_map (Aws.Json.lookup j "LocalZoneGroup") String.of_json
  ; coordinates =
      Aws.Util.option_map (Aws.Json.lookup j "Coordinates") Coordinates.of_json
  ; bias = Aws.Util.option_map (Aws.Json.lookup j "Bias") Integer.of_json
  }
