type t =
  { scale_up_modifications : NodeTypeList.t
  ; scale_down_modifications : NodeTypeList.t
  }

let make ?(scale_up_modifications = []) ?(scale_down_modifications = []) () =
  { scale_up_modifications; scale_down_modifications }

let parse xml =
  Some
    { scale_up_modifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ScaleUpModifications" xml)
             NodeTypeList.parse)
    ; scale_down_modifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ScaleDownModifications" xml)
             NodeTypeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ScaleDownModifications.member"
              , NodeTypeList.to_query v.scale_down_modifications ))
       ; Some
           (Aws.Query.Pair
              ( "ScaleUpModifications.member"
              , NodeTypeList.to_query v.scale_up_modifications ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ScaleDownModifications", NodeTypeList.to_json v.scale_down_modifications)
       ; Some ("ScaleUpModifications", NodeTypeList.to_json v.scale_up_modifications)
       ])

let of_json j =
  { scale_up_modifications =
      NodeTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ScaleUpModifications"))
  ; scale_down_modifications =
      NodeTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ScaleDownModifications"))
  }
