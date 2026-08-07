open Aws.BaseTypes

type t =
  { trails : Trails.t
  ; next_token : String.t option
  }

let make ?(trails = []) ?next_token () = { trails; next_token }

let parse xml =
  Some
    { trails =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Trails" xml) Trails.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Trails.member", Trails.to_query v.trails))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Trails", Trails.to_json v.trails)
       ])

let of_json j =
  { trails = Trails.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Trails"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
