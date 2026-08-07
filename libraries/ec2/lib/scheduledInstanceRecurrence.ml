open Aws.BaseTypes

type t =
  { frequency : String.t option
  ; interval : Integer.t option
  ; occurrence_day_set : OccurrenceDaySet.t
  ; occurrence_relative_to_end : Boolean.t option
  ; occurrence_unit : String.t option
  }

let make
    ?frequency
    ?interval
    ?(occurrence_day_set = [])
    ?occurrence_relative_to_end
    ?occurrence_unit
    () =
  { frequency; interval; occurrence_day_set; occurrence_relative_to_end; occurrence_unit }

let parse xml =
  Some
    { frequency = Aws.Util.option_bind (Aws.Xml.member "frequency" xml) String.parse
    ; interval = Aws.Util.option_bind (Aws.Xml.member "interval" xml) Integer.parse
    ; occurrence_day_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "occurrenceDaySet" xml)
             OccurrenceDaySet.parse)
    ; occurrence_relative_to_end =
        Aws.Util.option_bind (Aws.Xml.member "occurrenceRelativeToEnd" xml) Boolean.parse
    ; occurrence_unit =
        Aws.Util.option_bind (Aws.Xml.member "occurrenceUnit" xml) String.parse
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
              ("OccurrenceDaySet", OccurrenceDaySet.to_query v.occurrence_day_set))
       ; Aws.Util.option_map v.interval (fun f ->
             Aws.Query.Pair ("Interval", Integer.to_query f))
       ; Aws.Util.option_map v.frequency (fun f ->
             Aws.Query.Pair ("Frequency", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.occurrence_unit (fun f ->
             "occurrenceUnit", String.to_json f)
       ; Aws.Util.option_map v.occurrence_relative_to_end (fun f ->
             "occurrenceRelativeToEnd", Boolean.to_json f)
       ; Some ("occurrenceDaySet", OccurrenceDaySet.to_json v.occurrence_day_set)
       ; Aws.Util.option_map v.interval (fun f -> "interval", Integer.to_json f)
       ; Aws.Util.option_map v.frequency (fun f -> "frequency", String.to_json f)
       ])

let of_json j =
  { frequency = Aws.Util.option_map (Aws.Json.lookup j "frequency") String.of_json
  ; interval = Aws.Util.option_map (Aws.Json.lookup j "interval") Integer.of_json
  ; occurrence_day_set =
      OccurrenceDaySet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "occurrenceDaySet"))
  ; occurrence_relative_to_end =
      Aws.Util.option_map (Aws.Json.lookup j "occurrenceRelativeToEnd") Boolean.of_json
  ; occurrence_unit =
      Aws.Util.option_map (Aws.Json.lookup j "occurrenceUnit") String.of_json
  }
