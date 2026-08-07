open Aws.BaseTypes

type t =
  { bucket : String.t option
  ; key : String.t option
  }

let make ?bucket ?key () = { bucket; key }

let parse xml =
  Some
    { bucket = Aws.Util.option_bind (Aws.Xml.member "Bucket" xml) String.parse
    ; key = Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key (fun f -> Aws.Query.Pair ("Key", String.to_query f))
       ; Aws.Util.option_map v.bucket (fun f ->
             Aws.Query.Pair ("Bucket", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key (fun f -> "Key", String.to_json f)
       ; Aws.Util.option_map v.bucket (fun f -> "Bucket", String.to_json f)
       ])

let of_json j =
  { bucket = Aws.Util.option_map (Aws.Json.lookup j "Bucket") String.of_json
  ; key = Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json
  }
