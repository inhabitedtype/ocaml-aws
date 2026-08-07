open Aws.BaseTypes

type t =
  { dashboard_name : String.t
  ; dashboard_body : String.t
  ; tags : TagList.t
  }

let make ~dashboard_name ~dashboard_body ?(tags = []) () =
  { dashboard_name; dashboard_body; tags }

let parse xml =
  Some
    { dashboard_name =
        Aws.Xml.required
          "DashboardName"
          (Aws.Util.option_bind (Aws.Xml.member "DashboardName" xml) String.parse)
    ; dashboard_body =
        Aws.Xml.required
          "DashboardBody"
          (Aws.Util.option_bind (Aws.Xml.member "DashboardBody" xml) String.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("DashboardBody", String.to_query v.dashboard_body))
       ; Some (Aws.Query.Pair ("DashboardName", String.to_query v.dashboard_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some ("DashboardBody", String.to_json v.dashboard_body)
       ; Some ("DashboardName", String.to_json v.dashboard_name)
       ])

let of_json j =
  { dashboard_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardName"))
  ; dashboard_body =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardBody"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
