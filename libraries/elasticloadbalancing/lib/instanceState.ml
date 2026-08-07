open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; state : String.t option
  ; reason_code : String.t option
  ; description : String.t option
  }

let make ?instance_id ?state ?reason_code ?description () =
  { instance_id; state; reason_code; description }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse
    ; reason_code = Aws.Util.option_bind (Aws.Xml.member "ReasonCode" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.reason_code (fun f ->
             Aws.Query.Pair ("ReasonCode", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.reason_code (fun f -> "ReasonCode", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "State", String.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "State") String.of_json
  ; reason_code = Aws.Util.option_map (Aws.Json.lookup j "ReasonCode") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  }
