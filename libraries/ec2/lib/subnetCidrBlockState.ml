open Aws.BaseTypes

type t =
  { state : SubnetCidrBlockStateCode.t option
  ; status_message : String.t option
  }

let make ?state ?status_message () = { state; status_message }

let parse xml =
  Some
    { state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) SubnetCidrBlockStateCode.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", SubnetCidrBlockStateCode.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", SubnetCidrBlockStateCode.to_json f)
       ])

let of_json j =
  { state =
      Aws.Util.option_map (Aws.Json.lookup j "state") SubnetCidrBlockStateCode.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  }
