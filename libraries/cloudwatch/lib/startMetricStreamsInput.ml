type t = { names : MetricStreamNames.t }

let make ~names () = { names }

let parse xml =
  Some
    { names =
        Aws.Xml.required
          "Names"
          (Aws.Util.option_bind (Aws.Xml.member "Names" xml) MetricStreamNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Names.member", MetricStreamNames.to_query v.names)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Names", MetricStreamNames.to_json v.names) ])

let of_json j =
  { names = MetricStreamNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Names"))
  }
