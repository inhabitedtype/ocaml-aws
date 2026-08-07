type t = { status : OTelEnrichmentStatus.t }

let make ~status () = { status }

let parse xml =
  Some
    { status =
        Aws.Xml.required
          "Status"
          (Aws.Util.option_bind (Aws.Xml.member "Status" xml) OTelEnrichmentStatus.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Status", OTelEnrichmentStatus.to_query v.status)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("Status", OTelEnrichmentStatus.to_json v.status) ])

let of_json j =
  { status =
      OTelEnrichmentStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Status"))
  }
