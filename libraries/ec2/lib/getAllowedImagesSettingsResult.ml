open Aws.BaseTypes

type t =
  { state : String.t option
  ; image_criteria : ImageCriterionList.t
  ; managed_by : ManagedBy.t option
  }

let make ?state ?(image_criteria = []) ?managed_by () =
  { state; image_criteria; managed_by }

let parse xml =
  Some
    { state = Aws.Util.option_bind (Aws.Xml.member "state" xml) String.parse
    ; image_criteria =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "imageCriterionSet" xml)
             ImageCriterionList.parse)
    ; managed_by = Aws.Util.option_bind (Aws.Xml.member "managedBy" xml) ManagedBy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_by (fun f ->
             Aws.Query.Pair ("ManagedBy", ManagedBy.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ImageCriterionSet", ImageCriterionList.to_query v.image_criteria))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_by (fun f -> "managedBy", ManagedBy.to_json f)
       ; Some ("imageCriterionSet", ImageCriterionList.to_json v.image_criteria)
       ; Aws.Util.option_map v.state (fun f -> "state", String.to_json f)
       ])

let of_json j =
  { state = Aws.Util.option_map (Aws.Json.lookup j "state") String.of_json
  ; image_criteria =
      ImageCriterionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "imageCriterionSet"))
  ; managed_by = Aws.Util.option_map (Aws.Json.lookup j "managedBy") ManagedBy.of_json
  }
