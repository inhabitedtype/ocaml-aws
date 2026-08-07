type t =
  { action : ChangeAction.t
  ; resource_record_set : ResourceRecordSet.t
  }

let make ~action ~resource_record_set () = { action; resource_record_set }

let parse xml =
  Some
    { action =
        Aws.Xml.required
          "Action"
          (Aws.Util.option_bind (Aws.Xml.member "Action" xml) ChangeAction.parse)
    ; resource_record_set =
        Aws.Xml.required
          "ResourceRecordSet"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceRecordSet" xml)
             ResourceRecordSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ResourceRecordSet", ResourceRecordSet.to_query v.resource_record_set))
       ; Some (Aws.Query.Pair ("Action", ChangeAction.to_query v.action))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ResourceRecordSet", ResourceRecordSet.to_json v.resource_record_set)
       ; Some ("Action", ChangeAction.to_json v.action)
       ])

let of_json j =
  { action = ChangeAction.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Action"))
  ; resource_record_set =
      ResourceRecordSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceRecordSet"))
  }
