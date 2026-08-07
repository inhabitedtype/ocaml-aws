open Aws.BaseTypes

type t =
  { dashboards : Dashboards.t
  ; next_token : String.t option
  }

let make ?(dashboards = []) ?next_token () = { dashboards; next_token }

let parse xml =
  Some
    { dashboards =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Dashboards" xml) Dashboards.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Dashboards.member", Dashboards.to_query v.dashboards))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Dashboards", Dashboards.to_json v.dashboards)
       ])

let of_json j =
  { dashboards =
      Dashboards.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dashboards"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
