open Aws.BaseTypes

type t =
  { name : String.t
  ; source : String.t
  ; destinations : Destinations.t
  ; tags : TagsList.t
  }

let make ~name ~source ~destinations ?(tags = []) () =
  { name; source; destinations; tags }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; source =
        Aws.Xml.required
          "Source"
          (Aws.Util.option_bind (Aws.Xml.member "Source" xml) String.parse)
    ; destinations =
        Aws.Xml.required
          "Destinations"
          (Aws.Util.option_bind (Aws.Xml.member "Destinations" xml) Destinations.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagsList.to_query v.tags))
       ; Some
           (Aws.Query.Pair ("Destinations.member", Destinations.to_query v.destinations))
       ; Some (Aws.Query.Pair ("Source", String.to_query v.source))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagsList.to_json v.tags)
       ; Some ("Destinations", Destinations.to_json v.destinations)
       ; Some ("Source", String.to_json v.source)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; source = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Source"))
  ; destinations =
      Destinations.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Destinations"))
  ; tags = TagsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
