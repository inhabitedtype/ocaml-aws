type t = { dimension_condition : DimensionCondition.t option }

let make ?dimension_condition () = { dimension_condition }

let parse xml =
  Some
    { dimension_condition =
        Aws.Util.option_bind
          (Aws.Xml.member "DimensionCondition" xml)
          DimensionCondition.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dimension_condition (fun f ->
             Aws.Query.Pair ("DimensionCondition", DimensionCondition.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dimension_condition (fun f ->
             "DimensionCondition", DimensionCondition.to_json f)
       ])

let of_json j =
  { dimension_condition =
      Aws.Util.option_map
        (Aws.Json.lookup j "DimensionCondition")
        DimensionCondition.of_json
  }
