open Aws.BaseTypes

type t =
  { return : Boolean.t option
  ; key_pair_id : String.t option
  }

let make ?return ?key_pair_id () = { return; key_pair_id }

let parse xml =
  Some
    { return = Aws.Util.option_bind (Aws.Xml.member "return" xml) Boolean.parse
    ; key_pair_id = Aws.Util.option_bind (Aws.Xml.member "keyPairId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_pair_id (fun f ->
             Aws.Query.Pair ("KeyPairId", String.to_query f))
       ; Aws.Util.option_map v.return (fun f ->
             Aws.Query.Pair ("Return", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_pair_id (fun f -> "keyPairId", String.to_json f)
       ; Aws.Util.option_map v.return (fun f -> "return", Boolean.to_json f)
       ])

let of_json j =
  { return = Aws.Util.option_map (Aws.Json.lookup j "return") Boolean.of_json
  ; key_pair_id = Aws.Util.option_map (Aws.Json.lookup j "keyPairId") String.of_json
  }
