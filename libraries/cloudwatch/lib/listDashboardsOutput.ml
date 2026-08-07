open Aws.BaseTypes

type t =
  { dashboard_entries : DashboardEntries.t
  ; next_token : String.t option
  }

let make ?(dashboard_entries = []) ?next_token () = { dashboard_entries; next_token }

let parse xml =
  Some
    { dashboard_entries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DashboardEntries" xml)
             DashboardEntries.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DashboardEntries.member", DashboardEntries.to_query v.dashboard_entries))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("DashboardEntries", DashboardEntries.to_json v.dashboard_entries)
       ])

let of_json j =
  { dashboard_entries =
      DashboardEntries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardEntries"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
