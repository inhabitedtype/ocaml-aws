open Aws.BaseTypes

type t =
  { unit : RefreshScheduleFrequencyUnit.t option
  ; value : Integer.t option
  }

let make ?unit ?value () = { unit; value }

let parse xml =
  Some
    { unit =
        Aws.Util.option_bind
          (Aws.Xml.member "Unit" xml)
          RefreshScheduleFrequencyUnit.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", Integer.to_query f))
       ; Aws.Util.option_map v.unit (fun f ->
             Aws.Query.Pair ("Unit", RefreshScheduleFrequencyUnit.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f -> "Value", Integer.to_json f)
       ; Aws.Util.option_map v.unit (fun f ->
             "Unit", RefreshScheduleFrequencyUnit.to_json f)
       ])

let of_json j =
  { unit =
      Aws.Util.option_map (Aws.Json.lookup j "Unit") RefreshScheduleFrequencyUnit.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") Integer.of_json
  }
