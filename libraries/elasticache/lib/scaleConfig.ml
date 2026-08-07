open Aws.BaseTypes

type t =
  { scale_percentage : Integer.t option
  ; scale_interval_minutes : Integer.t option
  }

let make ?scale_percentage ?scale_interval_minutes () =
  { scale_percentage; scale_interval_minutes }

let parse xml =
  Some
    { scale_percentage =
        Aws.Util.option_bind (Aws.Xml.member "ScalePercentage" xml) Integer.parse
    ; scale_interval_minutes =
        Aws.Util.option_bind (Aws.Xml.member "ScaleIntervalMinutes" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scale_interval_minutes (fun f ->
             Aws.Query.Pair ("ScaleIntervalMinutes", Integer.to_query f))
       ; Aws.Util.option_map v.scale_percentage (fun f ->
             Aws.Query.Pair ("ScalePercentage", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scale_interval_minutes (fun f ->
             "ScaleIntervalMinutes", Integer.to_json f)
       ; Aws.Util.option_map v.scale_percentage (fun f ->
             "ScalePercentage", Integer.to_json f)
       ])

let of_json j =
  { scale_percentage =
      Aws.Util.option_map (Aws.Json.lookup j "ScalePercentage") Integer.of_json
  ; scale_interval_minutes =
      Aws.Util.option_map (Aws.Json.lookup j "ScaleIntervalMinutes") Integer.of_json
  }
