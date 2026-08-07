open Aws.BaseTypes

type t =
  { marker : String.t option
  ; source_regions : SourceRegionList.t
  }

let make ?marker ?(source_regions = []) () = { marker; source_regions }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; source_regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SourceRegions" xml)
             SourceRegionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("SourceRegions.member", SourceRegionList.to_query v.source_regions))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SourceRegions", SourceRegionList.to_json v.source_regions)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; source_regions =
      SourceRegionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceRegions"))
  }
