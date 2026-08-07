open Aws.BaseTypes

type t =
  { outpost_lags : OutpostLagSet.t
  ; next_token : String.t option
  }

let make ?(outpost_lags = []) ?next_token () = { outpost_lags; next_token }

let parse xml =
  Some
    { outpost_lags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "outpostLagSet" xml) OutpostLagSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("OutpostLagSet", OutpostLagSet.to_query v.outpost_lags))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("outpostLagSet", OutpostLagSet.to_json v.outpost_lags)
       ])

let of_json j =
  { outpost_lags =
      OutpostLagSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "outpostLagSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
