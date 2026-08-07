open Aws.BaseTypes

type t = { encoded_message : String.t }

let make ~encoded_message () = { encoded_message }

let parse xml =
  Some
    { encoded_message =
        Aws.Xml.required
          "EncodedMessage"
          (Aws.Util.option_bind (Aws.Xml.member "EncodedMessage" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("EncodedMessage", String.to_query v.encoded_message)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("EncodedMessage", String.to_json v.encoded_message) ])

let of_json j =
  { encoded_message =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EncodedMessage"))
  }
