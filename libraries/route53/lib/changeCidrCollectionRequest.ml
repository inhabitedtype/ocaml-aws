open Aws.BaseTypes

type t =
  { id : String.t
  ; collection_version : Long.t option
  ; changes : CidrCollectionChanges.t
  }

let make ~id ?collection_version ~changes () = { id; collection_version; changes }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "CidrCollectionId"
          (Aws.Util.option_bind (Aws.Xml.member "CidrCollectionId" xml) String.parse)
    ; collection_version =
        Aws.Util.option_bind (Aws.Xml.member "CollectionVersion" xml) Long.parse
    ; changes =
        Aws.Xml.required
          "Changes"
          (Aws.Util.option_bind
             (Aws.Xml.member "Changes" xml)
             CidrCollectionChanges.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Changes.member", CidrCollectionChanges.to_query v.changes))
       ; Aws.Util.option_map v.collection_version (fun f ->
             Aws.Query.Pair ("CollectionVersion", Long.to_query f))
       ; Some (Aws.Query.Pair ("CidrCollectionId", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Changes", CidrCollectionChanges.to_json v.changes)
       ; Aws.Util.option_map v.collection_version (fun f ->
             "CollectionVersion", Long.to_json f)
       ; Some ("CidrCollectionId", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CidrCollectionId"))
  ; collection_version =
      Aws.Util.option_map (Aws.Json.lookup j "CollectionVersion") Long.of_json
  ; changes =
      CidrCollectionChanges.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Changes"))
  }
