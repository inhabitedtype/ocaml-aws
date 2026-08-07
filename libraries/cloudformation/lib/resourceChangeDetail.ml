open Aws.BaseTypes

type t =
  { target : ResourceTargetDefinition.t option
  ; evaluation : EvaluationType.t option
  ; change_source : ChangeSource.t option
  ; causing_entity : String.t option
  }

let make ?target ?evaluation ?change_source ?causing_entity () =
  { target; evaluation; change_source; causing_entity }

let parse xml =
  Some
    { target =
        Aws.Util.option_bind (Aws.Xml.member "Target" xml) ResourceTargetDefinition.parse
    ; evaluation =
        Aws.Util.option_bind (Aws.Xml.member "Evaluation" xml) EvaluationType.parse
    ; change_source =
        Aws.Util.option_bind (Aws.Xml.member "ChangeSource" xml) ChangeSource.parse
    ; causing_entity =
        Aws.Util.option_bind (Aws.Xml.member "CausingEntity" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.causing_entity (fun f ->
             Aws.Query.Pair ("CausingEntity", String.to_query f))
       ; Aws.Util.option_map v.change_source (fun f ->
             Aws.Query.Pair ("ChangeSource", ChangeSource.to_query f))
       ; Aws.Util.option_map v.evaluation (fun f ->
             Aws.Query.Pair ("Evaluation", EvaluationType.to_query f))
       ; Aws.Util.option_map v.target (fun f ->
             Aws.Query.Pair ("Target", ResourceTargetDefinition.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.causing_entity (fun f -> "CausingEntity", String.to_json f)
       ; Aws.Util.option_map v.change_source (fun f ->
             "ChangeSource", ChangeSource.to_json f)
       ; Aws.Util.option_map v.evaluation (fun f ->
             "Evaluation", EvaluationType.to_json f)
       ; Aws.Util.option_map v.target (fun f ->
             "Target", ResourceTargetDefinition.to_json f)
       ])

let of_json j =
  { target =
      Aws.Util.option_map (Aws.Json.lookup j "Target") ResourceTargetDefinition.of_json
  ; evaluation =
      Aws.Util.option_map (Aws.Json.lookup j "Evaluation") EvaluationType.of_json
  ; change_source =
      Aws.Util.option_map (Aws.Json.lookup j "ChangeSource") ChangeSource.of_json
  ; causing_entity =
      Aws.Util.option_map (Aws.Json.lookup j "CausingEntity") String.of_json
  }
