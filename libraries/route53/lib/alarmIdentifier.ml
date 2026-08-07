open Aws.BaseTypes

type t =
  { region : CloudWatchRegion.t
  ; name : String.t
  }

let make ~region ~name () = { region; name }

let parse xml =
  Some
    { region =
        Aws.Xml.required
          "Region"
          (Aws.Util.option_bind (Aws.Xml.member "Region" xml) CloudWatchRegion.parse)
    ; name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ; Some (Aws.Query.Pair ("Region", CloudWatchRegion.to_query v.region))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Name", String.to_json v.name)
       ; Some ("Region", CloudWatchRegion.to_json v.region)
       ])

let of_json j =
  { region =
      CloudWatchRegion.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Region"))
  ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  }
