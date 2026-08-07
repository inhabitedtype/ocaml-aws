open Aws.BaseTypes

type t =
  { sample_count : Double.t
  ; sum : Double.t
  ; minimum : Double.t
  ; maximum : Double.t
  }

let make ~sample_count ~sum ~minimum ~maximum () = { sample_count; sum; minimum; maximum }

let parse xml =
  Some
    { sample_count =
        Aws.Xml.required
          "SampleCount"
          (Aws.Util.option_bind (Aws.Xml.member "SampleCount" xml) Double.parse)
    ; sum =
        Aws.Xml.required
          "Sum"
          (Aws.Util.option_bind (Aws.Xml.member "Sum" xml) Double.parse)
    ; minimum =
        Aws.Xml.required
          "Minimum"
          (Aws.Util.option_bind (Aws.Xml.member "Minimum" xml) Double.parse)
    ; maximum =
        Aws.Xml.required
          "Maximum"
          (Aws.Util.option_bind (Aws.Xml.member "Maximum" xml) Double.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Maximum", Double.to_query v.maximum))
       ; Some (Aws.Query.Pair ("Minimum", Double.to_query v.minimum))
       ; Some (Aws.Query.Pair ("Sum", Double.to_query v.sum))
       ; Some (Aws.Query.Pair ("SampleCount", Double.to_query v.sample_count))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Maximum", Double.to_json v.maximum)
       ; Some ("Minimum", Double.to_json v.minimum)
       ; Some ("Sum", Double.to_json v.sum)
       ; Some ("SampleCount", Double.to_json v.sample_count)
       ])

let of_json j =
  { sample_count =
      Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SampleCount"))
  ; sum = Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Sum"))
  ; minimum = Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Minimum"))
  ; maximum = Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Maximum"))
  }
