type t = { adjustment_types : AdjustmentTypes.t }

let make ?(adjustment_types = []) () = { adjustment_types }

let parse xml =
  Some
    { adjustment_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AdjustmentTypes" xml)
             AdjustmentTypes.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AdjustmentTypes.member", AdjustmentTypes.to_query v.adjustment_types))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("AdjustmentTypes", AdjustmentTypes.to_json v.adjustment_types) ])

let of_json j =
  { adjustment_types =
      AdjustmentTypes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdjustmentTypes"))
  }
