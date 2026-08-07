open Aws.BaseTypes

type t =
  { min_healthy_percentage : Integer.t option
  ; max_healthy_percentage : Integer.t option
  }

let make ?min_healthy_percentage ?max_healthy_percentage () =
  { min_healthy_percentage; max_healthy_percentage }

let parse xml =
  Some
    { min_healthy_percentage =
        Aws.Util.option_bind (Aws.Xml.member "MinHealthyPercentage" xml) Integer.parse
    ; max_healthy_percentage =
        Aws.Util.option_bind (Aws.Xml.member "MaxHealthyPercentage" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_healthy_percentage (fun f ->
             Aws.Query.Pair ("MaxHealthyPercentage", Integer.to_query f))
       ; Aws.Util.option_map v.min_healthy_percentage (fun f ->
             Aws.Query.Pair ("MinHealthyPercentage", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_healthy_percentage (fun f ->
             "MaxHealthyPercentage", Integer.to_json f)
       ; Aws.Util.option_map v.min_healthy_percentage (fun f ->
             "MinHealthyPercentage", Integer.to_json f)
       ])

let of_json j =
  { min_healthy_percentage =
      Aws.Util.option_map (Aws.Json.lookup j "MinHealthyPercentage") Integer.of_json
  ; max_healthy_percentage =
      Aws.Util.option_map (Aws.Json.lookup j "MaxHealthyPercentage") Integer.of_json
  }
