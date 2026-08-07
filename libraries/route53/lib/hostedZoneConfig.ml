open Aws.BaseTypes

type t =
  { comment : String.t option
  ; private_zone : Boolean.t option
  }

let make ?comment ?private_zone () = { comment; private_zone }

let parse xml =
  Some
    { comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
    ; private_zone = Aws.Util.option_bind (Aws.Xml.member "PrivateZone" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.private_zone (fun f ->
             Aws.Query.Pair ("PrivateZone", Boolean.to_query f))
       ; Aws.Util.option_map v.comment (fun f ->
             Aws.Query.Pair ("Comment", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.private_zone (fun f -> "PrivateZone", Boolean.to_json f)
       ; Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
       ])

let of_json j =
  { comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
  ; private_zone = Aws.Util.option_map (Aws.Json.lookup j "PrivateZone") Boolean.of_json
  }
