open Aws.BaseTypes

type t =
  { trail_a_r_n : String.t option
  ; name : String.t option
  ; home_region : String.t option
  }

let make ?trail_a_r_n ?name ?home_region () = { trail_a_r_n; name; home_region }

let parse xml =
  Some
    { trail_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TrailARN" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; home_region = Aws.Util.option_bind (Aws.Xml.member "HomeRegion" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.home_region (fun f ->
             Aws.Query.Pair ("HomeRegion", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.trail_a_r_n (fun f ->
             Aws.Query.Pair ("TrailARN", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.home_region (fun f -> "HomeRegion", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.trail_a_r_n (fun f -> "TrailARN", String.to_json f)
       ])

let of_json j =
  { trail_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TrailARN") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; home_region = Aws.Util.option_map (Aws.Json.lookup j "HomeRegion") String.of_json
  }
