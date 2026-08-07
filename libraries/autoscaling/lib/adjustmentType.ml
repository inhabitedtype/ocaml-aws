open Aws.BaseTypes

type t = { adjustment_type : String.t option }

let make ?adjustment_type () = { adjustment_type }

let parse xml =
  Some
    { adjustment_type =
        Aws.Util.option_bind (Aws.Xml.member "AdjustmentType" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.adjustment_type (fun f ->
             Aws.Query.Pair ("AdjustmentType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.adjustment_type (fun f ->
             "AdjustmentType", String.to_json f)
       ])

let of_json j =
  { adjustment_type =
      Aws.Util.option_map (Aws.Json.lookup j "AdjustmentType") String.of_json
  }
