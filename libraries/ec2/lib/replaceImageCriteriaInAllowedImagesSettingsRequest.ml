open Aws.BaseTypes

type t =
  { image_criteria : ImageCriterionRequestList.t
  ; dry_run : Boolean.t option
  }

let make ?(image_criteria = []) ?dry_run () = { image_criteria; dry_run }

let parse xml =
  Some
    { image_criteria =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ImageCriterion" xml)
             ImageCriterionRequestList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ImageCriterion", ImageCriterionRequestList.to_query v.image_criteria))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("ImageCriterion", ImageCriterionRequestList.to_json v.image_criteria)
       ])

let of_json j =
  { image_criteria =
      ImageCriterionRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageCriterion"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
