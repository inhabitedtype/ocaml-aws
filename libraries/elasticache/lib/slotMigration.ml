open Aws.BaseTypes

type t = { progress_percentage : Double.t option }

let make ?progress_percentage () = { progress_percentage }

let parse xml =
  Some
    { progress_percentage =
        Aws.Util.option_bind (Aws.Xml.member "ProgressPercentage" xml) Double.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.progress_percentage (fun f ->
             Aws.Query.Pair ("ProgressPercentage", Double.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.progress_percentage (fun f ->
             "ProgressPercentage", Double.to_json f)
       ])

let of_json j =
  { progress_percentage =
      Aws.Util.option_map (Aws.Json.lookup j "ProgressPercentage") Double.of_json
  }
