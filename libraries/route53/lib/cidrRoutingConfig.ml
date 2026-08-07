open Aws.BaseTypes

type t =
  { collection_id : String.t
  ; location_name : String.t
  }

let make ~collection_id ~location_name () = { collection_id; location_name }

let parse xml =
  Some
    { collection_id =
        Aws.Xml.required
          "CollectionId"
          (Aws.Util.option_bind (Aws.Xml.member "CollectionId" xml) String.parse)
    ; location_name =
        Aws.Xml.required
          "LocationName"
          (Aws.Util.option_bind (Aws.Xml.member "LocationName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("LocationName", String.to_query v.location_name))
       ; Some (Aws.Query.Pair ("CollectionId", String.to_query v.collection_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LocationName", String.to_json v.location_name)
       ; Some ("CollectionId", String.to_json v.collection_id)
       ])

let of_json j =
  { collection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CollectionId"))
  ; location_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LocationName"))
  }
