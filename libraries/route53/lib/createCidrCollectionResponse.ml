open Aws.BaseTypes

type t =
  { collection : CidrCollection.t option
  ; location : String.t option
  }

let make ?collection ?location () = { collection; location }

let parse xml =
  Some
    { collection =
        Aws.Util.option_bind (Aws.Xml.member "Collection" xml) CidrCollection.parse
    ; location = Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.location (fun f ->
             Aws.Query.Pair ("Location", String.to_query f))
       ; Aws.Util.option_map v.collection (fun f ->
             Aws.Query.Pair ("Collection", CidrCollection.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.location (fun f -> "Location", String.to_json f)
       ; Aws.Util.option_map v.collection (fun f ->
             "Collection", CidrCollection.to_json f)
       ])

let of_json j =
  { collection =
      Aws.Util.option_map (Aws.Json.lookup j "Collection") CidrCollection.of_json
  ; location = Aws.Util.option_map (Aws.Json.lookup j "Location") String.of_json
  }
