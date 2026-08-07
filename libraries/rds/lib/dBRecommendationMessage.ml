type t = { d_b_recommendation : DBRecommendation.t option }

let make ?d_b_recommendation () = { d_b_recommendation }

let parse xml =
  Some
    { d_b_recommendation =
        Aws.Util.option_bind
          (Aws.Xml.member "DBRecommendation" xml)
          DBRecommendation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_recommendation (fun f ->
             Aws.Query.Pair ("DBRecommendation", DBRecommendation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_recommendation (fun f ->
             "DBRecommendation", DBRecommendation.to_json f)
       ])

let of_json j =
  { d_b_recommendation =
      Aws.Util.option_map (Aws.Json.lookup j "DBRecommendation") DBRecommendation.of_json
  }
