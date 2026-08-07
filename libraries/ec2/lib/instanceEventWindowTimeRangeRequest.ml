open Aws.BaseTypes

type t =
  { start_week_day : WeekDay.t option
  ; start_hour : Integer.t option
  ; end_week_day : WeekDay.t option
  ; end_hour : Integer.t option
  }

let make ?start_week_day ?start_hour ?end_week_day ?end_hour () =
  { start_week_day; start_hour; end_week_day; end_hour }

let parse xml =
  Some
    { start_week_day =
        Aws.Util.option_bind (Aws.Xml.member "StartWeekDay" xml) WeekDay.parse
    ; start_hour = Aws.Util.option_bind (Aws.Xml.member "StartHour" xml) Integer.parse
    ; end_week_day = Aws.Util.option_bind (Aws.Xml.member "EndWeekDay" xml) WeekDay.parse
    ; end_hour = Aws.Util.option_bind (Aws.Xml.member "EndHour" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.end_hour (fun f ->
             Aws.Query.Pair ("EndHour", Integer.to_query f))
       ; Aws.Util.option_map v.end_week_day (fun f ->
             Aws.Query.Pair ("EndWeekDay", WeekDay.to_query f))
       ; Aws.Util.option_map v.start_hour (fun f ->
             Aws.Query.Pair ("StartHour", Integer.to_query f))
       ; Aws.Util.option_map v.start_week_day (fun f ->
             Aws.Query.Pair ("StartWeekDay", WeekDay.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.end_hour (fun f -> "EndHour", Integer.to_json f)
       ; Aws.Util.option_map v.end_week_day (fun f -> "EndWeekDay", WeekDay.to_json f)
       ; Aws.Util.option_map v.start_hour (fun f -> "StartHour", Integer.to_json f)
       ; Aws.Util.option_map v.start_week_day (fun f -> "StartWeekDay", WeekDay.to_json f)
       ])

let of_json j =
  { start_week_day =
      Aws.Util.option_map (Aws.Json.lookup j "StartWeekDay") WeekDay.of_json
  ; start_hour = Aws.Util.option_map (Aws.Json.lookup j "StartHour") Integer.of_json
  ; end_week_day = Aws.Util.option_map (Aws.Json.lookup j "EndWeekDay") WeekDay.of_json
  ; end_hour = Aws.Util.option_map (Aws.Json.lookup j "EndHour") Integer.of_json
  }
