open Aws.BaseTypes

type t = { decoded_message : String.t option }

let make ?decoded_message () = { decoded_message }

let parse xml =
  Some
    { decoded_message =
        Aws.Util.option_bind (Aws.Xml.member "DecodedMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.decoded_message (fun f ->
             Aws.Query.Pair ("DecodedMessage", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.decoded_message (fun f ->
             "DecodedMessage", String.to_json f)
       ])

let of_json j =
  { decoded_message =
      Aws.Util.option_map (Aws.Json.lookup j "DecodedMessage") String.of_json
  }
