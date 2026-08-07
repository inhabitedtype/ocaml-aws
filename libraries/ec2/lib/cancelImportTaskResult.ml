open Aws.BaseTypes

type t =
  { import_task_id : String.t option
  ; previous_state : String.t option
  ; state : String.t option
  }

let make ?import_task_id ?previous_state ?state () =
  { import_task_id; previous_state; state }

let parse xml =
  Some
    { import_task_id =
        Aws.Util.option_bind (Aws.Xml.member "importTaskId" xml) String.parse
    ; previous_state =
        Aws.Util.option_bind (Aws.Xml.member "previousState" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.previous_state (fun f ->
             Aws.Query.Pair ("PreviousState", String.to_query f))
       ; Aws.Util.option_map v.import_task_id (fun f ->
             Aws.Query.Pair ("ImportTaskId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f -> "state", String.to_json f)
       ; Aws.Util.option_map v.previous_state (fun f -> "previousState", String.to_json f)
       ; Aws.Util.option_map v.import_task_id (fun f -> "importTaskId", String.to_json f)
       ])

let of_json j =
  { import_task_id = Aws.Util.option_map (Aws.Json.lookup j "importTaskId") String.of_json
  ; previous_state =
      Aws.Util.option_map (Aws.Json.lookup j "previousState") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") String.of_json
  }
