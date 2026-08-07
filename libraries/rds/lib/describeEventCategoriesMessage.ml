open Aws.BaseTypes

type t =
  { source_type : String.t option
  ; filters : FilterList.t
  }

let make ?source_type ?(filters = []) () = { source_type; filters }

let parse xml =
  Some
    { source_type = Aws.Util.option_bind (Aws.Xml.member "SourceType" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filters.member", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.source_type (fun f ->
             Aws.Query.Pair ("SourceType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.source_type (fun f -> "SourceType", String.to_json f)
       ])

let of_json j =
  { source_type = Aws.Util.option_map (Aws.Json.lookup j "SourceType") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  }
