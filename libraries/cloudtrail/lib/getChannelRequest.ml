open Aws.BaseTypes

type t = { channel : String.t }

let make ~channel () = { channel }

let parse xml =
  Some
    { channel =
        Aws.Xml.required
          "Channel"
          (Aws.Util.option_bind (Aws.Xml.member "Channel" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Channel", String.to_query v.channel)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Channel", String.to_json v.channel) ])

let of_json j =
  { channel = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Channel")) }
