open Aws.BaseTypes

type t =
  { timestamp : DateTime.t option
  ; sample_count : Double.t option
  ; average : Double.t option
  ; sum : Double.t option
  ; minimum : Double.t option
  ; maximum : Double.t option
  ; unit : StandardUnit.t option
  ; extended_statistics : DatapointValueMap.t option
  }

let make
    ?timestamp
    ?sample_count
    ?average
    ?sum
    ?minimum
    ?maximum
    ?unit
    ?extended_statistics
    () =
  { timestamp; sample_count; average; sum; minimum; maximum; unit; extended_statistics }

let parse xml =
  Some
    { timestamp = Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse
    ; sample_count = Aws.Util.option_bind (Aws.Xml.member "SampleCount" xml) Double.parse
    ; average = Aws.Util.option_bind (Aws.Xml.member "Average" xml) Double.parse
    ; sum = Aws.Util.option_bind (Aws.Xml.member "Sum" xml) Double.parse
    ; minimum = Aws.Util.option_bind (Aws.Xml.member "Minimum" xml) Double.parse
    ; maximum = Aws.Util.option_bind (Aws.Xml.member "Maximum" xml) Double.parse
    ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) StandardUnit.parse
    ; extended_statistics =
        Aws.Util.option_bind
          (Aws.Xml.member "ExtendedStatistics" xml)
          DatapointValueMap.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.extended_statistics (fun f ->
             Aws.Query.Pair ("ExtendedStatistics", DatapointValueMap.to_query f))
       ; Aws.Util.option_map v.unit (fun f ->
             Aws.Query.Pair ("Unit", StandardUnit.to_query f))
       ; Aws.Util.option_map v.maximum (fun f ->
             Aws.Query.Pair ("Maximum", Double.to_query f))
       ; Aws.Util.option_map v.minimum (fun f ->
             Aws.Query.Pair ("Minimum", Double.to_query f))
       ; Aws.Util.option_map v.sum (fun f -> Aws.Query.Pair ("Sum", Double.to_query f))
       ; Aws.Util.option_map v.average (fun f ->
             Aws.Query.Pair ("Average", Double.to_query f))
       ; Aws.Util.option_map v.sample_count (fun f ->
             Aws.Query.Pair ("SampleCount", Double.to_query f))
       ; Aws.Util.option_map v.timestamp (fun f ->
             Aws.Query.Pair ("Timestamp", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.extended_statistics (fun f ->
             "ExtendedStatistics", DatapointValueMap.to_json f)
       ; Aws.Util.option_map v.unit (fun f -> "Unit", StandardUnit.to_json f)
       ; Aws.Util.option_map v.maximum (fun f -> "Maximum", Double.to_json f)
       ; Aws.Util.option_map v.minimum (fun f -> "Minimum", Double.to_json f)
       ; Aws.Util.option_map v.sum (fun f -> "Sum", Double.to_json f)
       ; Aws.Util.option_map v.average (fun f -> "Average", Double.to_json f)
       ; Aws.Util.option_map v.sample_count (fun f -> "SampleCount", Double.to_json f)
       ; Aws.Util.option_map v.timestamp (fun f -> "Timestamp", DateTime.to_json f)
       ])

let of_json j =
  { timestamp = Aws.Util.option_map (Aws.Json.lookup j "Timestamp") DateTime.of_json
  ; sample_count = Aws.Util.option_map (Aws.Json.lookup j "SampleCount") Double.of_json
  ; average = Aws.Util.option_map (Aws.Json.lookup j "Average") Double.of_json
  ; sum = Aws.Util.option_map (Aws.Json.lookup j "Sum") Double.of_json
  ; minimum = Aws.Util.option_map (Aws.Json.lookup j "Minimum") Double.of_json
  ; maximum = Aws.Util.option_map (Aws.Json.lookup j "Maximum") Double.of_json
  ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
  ; extended_statistics =
      Aws.Util.option_map
        (Aws.Json.lookup j "ExtendedStatistics")
        DatapointValueMap.of_json
  }
