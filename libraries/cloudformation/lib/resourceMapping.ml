type t =
  { source : ResourceLocation.t
  ; destination : ResourceLocation.t
  }

let make ~source ~destination () = { source; destination }

let parse xml =
  Some
    { source =
        Aws.Xml.required
          "Source"
          (Aws.Util.option_bind (Aws.Xml.member "Source" xml) ResourceLocation.parse)
    ; destination =
        Aws.Xml.required
          "Destination"
          (Aws.Util.option_bind (Aws.Xml.member "Destination" xml) ResourceLocation.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Destination", ResourceLocation.to_query v.destination))
       ; Some (Aws.Query.Pair ("Source", ResourceLocation.to_query v.source))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Destination", ResourceLocation.to_json v.destination)
       ; Some ("Source", ResourceLocation.to_json v.source)
       ])

let of_json j =
  { source =
      ResourceLocation.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Source"))
  ; destination =
      ResourceLocation.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Destination"))
  }
