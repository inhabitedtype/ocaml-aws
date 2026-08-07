open Aws.BaseTypes

type t =
  { use_long_ids_aggregated : Boolean.t option
  ; statuses : IdFormatList.t
  }

let make ?use_long_ids_aggregated ?(statuses = []) () =
  { use_long_ids_aggregated; statuses }

let parse xml =
  Some
    { use_long_ids_aggregated =
        Aws.Util.option_bind (Aws.Xml.member "useLongIdsAggregated" xml) Boolean.parse
    ; statuses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "statusSet" xml) IdFormatList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("StatusSet", IdFormatList.to_query v.statuses))
       ; Aws.Util.option_map v.use_long_ids_aggregated (fun f ->
             Aws.Query.Pair ("UseLongIdsAggregated", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("statusSet", IdFormatList.to_json v.statuses)
       ; Aws.Util.option_map v.use_long_ids_aggregated (fun f ->
             "useLongIdsAggregated", Boolean.to_json f)
       ])

let of_json j =
  { use_long_ids_aggregated =
      Aws.Util.option_map (Aws.Json.lookup j "useLongIdsAggregated") Boolean.of_json
  ; statuses =
      IdFormatList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "statusSet"))
  }
