open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; entries : MetricStreamEntries.t
  }

let make ?next_token ?(entries = []) () = { next_token; entries }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; entries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Entries" xml) MetricStreamEntries.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Entries.member", MetricStreamEntries.to_query v.entries))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Entries", MetricStreamEntries.to_json v.entries)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; entries =
      MetricStreamEntries.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Entries"))
  }
