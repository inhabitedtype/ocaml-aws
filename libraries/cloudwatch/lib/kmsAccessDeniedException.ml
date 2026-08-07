open Aws.BaseTypes

type t = { message : String.t }

let make ~message () = { message }

let parse xml =
  Some
    { message =
        Aws.Xml.required
          "Message"
          (Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Message", String.to_query v.message)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Message", String.to_json v.message) ])

let of_json j =
  { message = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Message")) }
