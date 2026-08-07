open Aws.BaseTypes

type t =
  { latitude : String.t
  ; longitude : String.t
  }

let make ~latitude ~longitude () = { latitude; longitude }

let parse xml =
  Some
    { latitude =
        Aws.Xml.required
          "Latitude"
          (Aws.Util.option_bind (Aws.Xml.member "Latitude" xml) String.parse)
    ; longitude =
        Aws.Xml.required
          "Longitude"
          (Aws.Util.option_bind (Aws.Xml.member "Longitude" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Longitude", String.to_query v.longitude))
       ; Some (Aws.Query.Pair ("Latitude", String.to_query v.latitude))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Longitude", String.to_json v.longitude)
       ; Some ("Latitude", String.to_json v.latitude)
       ])

let of_json j =
  { latitude = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Latitude"))
  ; longitude = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Longitude"))
  }
