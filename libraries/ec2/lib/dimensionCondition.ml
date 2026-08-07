type t =
  { dimension : FilterByDimension.t option
  ; comparison : Comparison.t option
  ; values : ConditionValueList.t
  }

let make ?dimension ?comparison ?(values = []) () = { dimension; comparison; values }

let parse xml =
  Some
    { dimension =
        Aws.Util.option_bind (Aws.Xml.member "Dimension" xml) FilterByDimension.parse
    ; comparison = Aws.Util.option_bind (Aws.Xml.member "Comparison" xml) Comparison.parse
    ; values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Value" xml) ConditionValueList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Value", ConditionValueList.to_query v.values))
       ; Aws.Util.option_map v.comparison (fun f ->
             Aws.Query.Pair ("Comparison", Comparison.to_query f))
       ; Aws.Util.option_map v.dimension (fun f ->
             Aws.Query.Pair ("Dimension", FilterByDimension.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Value", ConditionValueList.to_json v.values)
       ; Aws.Util.option_map v.comparison (fun f -> "Comparison", Comparison.to_json f)
       ; Aws.Util.option_map v.dimension (fun f ->
             "Dimension", FilterByDimension.to_json f)
       ])

let of_json j =
  { dimension =
      Aws.Util.option_map (Aws.Json.lookup j "Dimension") FilterByDimension.of_json
  ; comparison = Aws.Util.option_map (Aws.Json.lookup j "Comparison") Comparison.of_json
  ; values =
      ConditionValueList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
  }
