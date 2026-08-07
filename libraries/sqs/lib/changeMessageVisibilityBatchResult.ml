type t =
  { successful : ChangeMessageVisibilityBatchResultEntryList.t
  ; failed : BatchResultErrorEntryList.t
  }

let make ~successful ~failed () = { successful; failed }

let parse xml =
  Some
    { successful =
        Aws.Xml.required
          "Successful"
          (ChangeMessageVisibilityBatchResultEntryList.parse xml)
    ; failed = Aws.Xml.required "Failed" (BatchResultErrorEntryList.parse xml)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Failed.member", BatchResultErrorEntryList.to_query v.failed))
       ; Some
           (Aws.Query.Pair
              ( "Successful.member"
              , ChangeMessageVisibilityBatchResultEntryList.to_query v.successful ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Failed", BatchResultErrorEntryList.to_json v.failed)
       ; Some
           ("Successful", ChangeMessageVisibilityBatchResultEntryList.to_json v.successful)
       ])

let of_json j =
  { successful =
      ChangeMessageVisibilityBatchResultEntryList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Successful"))
  ; failed =
      BatchResultErrorEntryList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Failed"))
  }
