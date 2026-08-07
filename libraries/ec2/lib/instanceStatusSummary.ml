type t =
  { details : InstanceStatusDetailsList.t
  ; status : SummaryStatus.t
  }

let make ?(details = []) ~status () = { details; status }

let parse xml =
  Some
    { details =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "details" xml)
             InstanceStatusDetailsList.parse)
    ; status =
        Aws.Xml.required
          "status"
          (Aws.Util.option_bind (Aws.Xml.member "status" xml) SummaryStatus.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Status", SummaryStatus.to_query v.status))
       ; Some (Aws.Query.Pair ("Details", InstanceStatusDetailsList.to_query v.details))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("status", SummaryStatus.to_json v.status)
       ; Some ("details", InstanceStatusDetailsList.to_json v.details)
       ])

let of_json j =
  { details =
      InstanceStatusDetailsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "details"))
  ; status = SummaryStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "status"))
  }
