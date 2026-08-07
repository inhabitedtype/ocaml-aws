open Aws.BaseTypes

type t =
  { spot_placement_scores : SpotPlacementScores.t
  ; next_token : String.t option
  }

let make ?(spot_placement_scores = []) ?next_token () =
  { spot_placement_scores; next_token }

let parse xml =
  Some
    { spot_placement_scores =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "spotPlacementScoreSet" xml)
             SpotPlacementScores.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SpotPlacementScoreSet"
              , SpotPlacementScores.to_query v.spot_placement_scores ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ("spotPlacementScoreSet", SpotPlacementScores.to_json v.spot_placement_scores)
       ])

let of_json j =
  { spot_placement_scores =
      SpotPlacementScores.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "spotPlacementScoreSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
