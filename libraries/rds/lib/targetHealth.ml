open Aws.BaseTypes

type t =
  { state : TargetState.t option
  ; reason : TargetHealthReason.t option
  ; description : String.t option
  }

let make ?state ?reason ?description () = { state; reason; description }

let parse xml =
  Some
    { state = Aws.Util.option_bind (Aws.Xml.member "State" xml) TargetState.parse
    ; reason = Aws.Util.option_bind (Aws.Xml.member "Reason" xml) TargetHealthReason.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.reason (fun f ->
             Aws.Query.Pair ("Reason", TargetHealthReason.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TargetState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.reason (fun f -> "Reason", TargetHealthReason.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "State", TargetState.to_json f)
       ])

let of_json j =
  { state = Aws.Util.option_map (Aws.Json.lookup j "State") TargetState.of_json
  ; reason = Aws.Util.option_map (Aws.Json.lookup j "Reason") TargetHealthReason.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  }
