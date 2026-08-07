open Aws.BaseTypes

type t =
  { initialization_type : InitializationType.t option
  ; progress : Long.t option
  ; estimated_time_to_complete_in_seconds : Long.t option
  }

let make ?initialization_type ?progress ?estimated_time_to_complete_in_seconds () =
  { initialization_type; progress; estimated_time_to_complete_in_seconds }

let parse xml =
  Some
    { initialization_type =
        Aws.Util.option_bind
          (Aws.Xml.member "initializationType" xml)
          InitializationType.parse
    ; progress = Aws.Util.option_bind (Aws.Xml.member "progress" xml) Long.parse
    ; estimated_time_to_complete_in_seconds =
        Aws.Util.option_bind
          (Aws.Xml.member "estimatedTimeToCompleteInSeconds" xml)
          Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.estimated_time_to_complete_in_seconds (fun f ->
             Aws.Query.Pair ("EstimatedTimeToCompleteInSeconds", Long.to_query f))
       ; Aws.Util.option_map v.progress (fun f ->
             Aws.Query.Pair ("Progress", Long.to_query f))
       ; Aws.Util.option_map v.initialization_type (fun f ->
             Aws.Query.Pair ("InitializationType", InitializationType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.estimated_time_to_complete_in_seconds (fun f ->
             "estimatedTimeToCompleteInSeconds", Long.to_json f)
       ; Aws.Util.option_map v.progress (fun f -> "progress", Long.to_json f)
       ; Aws.Util.option_map v.initialization_type (fun f ->
             "initializationType", InitializationType.to_json f)
       ])

let of_json j =
  { initialization_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "initializationType")
        InitializationType.of_json
  ; progress = Aws.Util.option_map (Aws.Json.lookup j "progress") Long.of_json
  ; estimated_time_to_complete_in_seconds =
      Aws.Util.option_map
        (Aws.Json.lookup j "estimatedTimeToCompleteInSeconds")
        Long.of_json
  }
