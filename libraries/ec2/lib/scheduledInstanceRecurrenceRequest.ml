open Aws.BaseTypes

type t =
  { frequency : String.t option
  ; interval : Integer.t option
  ; occurrence_days : OccurrenceDayRequestSet.t
  ; occurrence_relative_to_end : Boolean.t option
  ; occurrence_unit : String.t option
  }

let make
    ?frequency
    ?interval
    ?(occurrence_days = [])
    ?occurrence_relative_to_end
    ?occurrence_unit
    () =
  { frequency; interval; occurrence_days; occurrence_relative_to_end; occurrence_unit }

let parse xml =
  Some
    { frequency = Aws.Util.option_bind (Aws.Xml.member "Frequency" xml) String.parse
    ; interval = Aws.Util.option_bind (Aws.Xml.member "Interval" xml) Integer.parse
    ; occurrence_days =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OccurrenceDay" xml)
             OccurrenceDayRequestSet.parse)
    ; occurrence_relative_to_end =
        Aws.Util.option_bind (Aws.Xml.member "OccurrenceRelativeToEnd" xml) Boolean.parse
    ; occurrence_unit =
        Aws.Util.option_bind (Aws.Xml.member "OccurrenceUnit" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.occurrence_unit (fun f ->
             Aws.Query.Pair ("OccurrenceUnit", String.to_query f))
       ; Aws.Util.option_map v.occurrence_relative_to_end (fun f ->
             Aws.Query.Pair ("OccurrenceRelativeToEnd", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("OccurrenceDay", OccurrenceDayRequestSet.to_query v.occurrence_days))
       ; Aws.Util.option_map v.interval (fun f ->
             Aws.Query.Pair ("Interval", Integer.to_query f))
       ; Aws.Util.option_map v.frequency (fun f ->
             Aws.Query.Pair ("Frequency", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.occurrence_unit (fun f ->
             "OccurrenceUnit", String.to_json f)
       ; Aws.Util.option_map v.occurrence_relative_to_end (fun f ->
             "OccurrenceRelativeToEnd", Boolean.to_json f)
       ; Some ("OccurrenceDay", OccurrenceDayRequestSet.to_json v.occurrence_days)
       ; Aws.Util.option_map v.interval (fun f -> "Interval", Integer.to_json f)
       ; Aws.Util.option_map v.frequency (fun f -> "Frequency", String.to_json f)
       ])

let of_json j =
  { frequency = Aws.Util.option_map (Aws.Json.lookup j "Frequency") String.of_json
  ; interval = Aws.Util.option_map (Aws.Json.lookup j "Interval") Integer.of_json
  ; occurrence_days =
      OccurrenceDayRequestSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OccurrenceDay"))
  ; occurrence_relative_to_end =
      Aws.Util.option_map (Aws.Json.lookup j "OccurrenceRelativeToEnd") Boolean.of_json
  ; occurrence_unit =
      Aws.Util.option_map (Aws.Json.lookup j "OccurrenceUnit") String.of_json
  }
