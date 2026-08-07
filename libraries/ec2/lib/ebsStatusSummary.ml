type t =
  { details : EbsStatusDetailsList.t
  ; status : SummaryStatus.t option
  }

let make ?(details = []) ?status () = { details; status }

let parse xml =
  Some
    { details =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "details" xml) EbsStatusDetailsList.parse)
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) SummaryStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", SummaryStatus.to_query f))
       ; Some (Aws.Query.Pair ("Details", EbsStatusDetailsList.to_query v.details))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "status", SummaryStatus.to_json f)
       ; Some ("details", EbsStatusDetailsList.to_json v.details)
       ])

let of_json j =
  { details =
      EbsStatusDetailsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "details"))
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") SummaryStatus.of_json
  }
