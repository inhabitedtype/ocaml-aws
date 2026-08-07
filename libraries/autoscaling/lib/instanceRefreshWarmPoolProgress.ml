open Aws.BaseTypes

type t =
  { percentage_complete : Integer.t option
  ; instances_to_update : Integer.t option
  }

let make ?percentage_complete ?instances_to_update () =
  { percentage_complete; instances_to_update }

let parse xml =
  Some
    { percentage_complete =
        Aws.Util.option_bind (Aws.Xml.member "PercentageComplete" xml) Integer.parse
    ; instances_to_update =
        Aws.Util.option_bind (Aws.Xml.member "InstancesToUpdate" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instances_to_update (fun f ->
             Aws.Query.Pair ("InstancesToUpdate", Integer.to_query f))
       ; Aws.Util.option_map v.percentage_complete (fun f ->
             Aws.Query.Pair ("PercentageComplete", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instances_to_update (fun f ->
             "InstancesToUpdate", Integer.to_json f)
       ; Aws.Util.option_map v.percentage_complete (fun f ->
             "PercentageComplete", Integer.to_json f)
       ])

let of_json j =
  { percentage_complete =
      Aws.Util.option_map (Aws.Json.lookup j "PercentageComplete") Integer.of_json
  ; instances_to_update =
      Aws.Util.option_map (Aws.Json.lookup j "InstancesToUpdate") Integer.of_json
  }
