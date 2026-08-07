open Aws.BaseTypes

type t =
  { frequency : RefreshScheduleFrequency.t option
  ; status : RefreshScheduleStatus.t option
  ; time_of_day : String.t option
  }

let make ?frequency ?status ?time_of_day () = { frequency; status; time_of_day }

let parse xml =
  Some
    { frequency =
        Aws.Util.option_bind
          (Aws.Xml.member "Frequency" xml)
          RefreshScheduleFrequency.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) RefreshScheduleStatus.parse
    ; time_of_day = Aws.Util.option_bind (Aws.Xml.member "TimeOfDay" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.time_of_day (fun f ->
             Aws.Query.Pair ("TimeOfDay", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", RefreshScheduleStatus.to_query f))
       ; Aws.Util.option_map v.frequency (fun f ->
             Aws.Query.Pair ("Frequency", RefreshScheduleFrequency.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.time_of_day (fun f -> "TimeOfDay", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", RefreshScheduleStatus.to_json f)
       ; Aws.Util.option_map v.frequency (fun f ->
             "Frequency", RefreshScheduleFrequency.to_json f)
       ])

let of_json j =
  { frequency =
      Aws.Util.option_map (Aws.Json.lookup j "Frequency") RefreshScheduleFrequency.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "Status") RefreshScheduleStatus.of_json
  ; time_of_day = Aws.Util.option_map (Aws.Json.lookup j "TimeOfDay") String.of_json
  }
