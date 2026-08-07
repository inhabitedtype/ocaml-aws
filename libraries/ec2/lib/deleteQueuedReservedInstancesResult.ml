type t =
  { successful_queued_purchase_deletions : SuccessfulQueuedPurchaseDeletionSet.t
  ; failed_queued_purchase_deletions : FailedQueuedPurchaseDeletionSet.t
  }

let make
    ?(successful_queued_purchase_deletions = [])
    ?(failed_queued_purchase_deletions = [])
    () =
  { successful_queued_purchase_deletions; failed_queued_purchase_deletions }

let parse xml =
  Some
    { successful_queued_purchase_deletions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "successfulQueuedPurchaseDeletionSet" xml)
             SuccessfulQueuedPurchaseDeletionSet.parse)
    ; failed_queued_purchase_deletions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "failedQueuedPurchaseDeletionSet" xml)
             FailedQueuedPurchaseDeletionSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "FailedQueuedPurchaseDeletionSet"
              , FailedQueuedPurchaseDeletionSet.to_query
                  v.failed_queued_purchase_deletions ))
       ; Some
           (Aws.Query.Pair
              ( "SuccessfulQueuedPurchaseDeletionSet"
              , SuccessfulQueuedPurchaseDeletionSet.to_query
                  v.successful_queued_purchase_deletions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "failedQueuedPurchaseDeletionSet"
           , FailedQueuedPurchaseDeletionSet.to_json v.failed_queued_purchase_deletions )
       ; Some
           ( "successfulQueuedPurchaseDeletionSet"
           , SuccessfulQueuedPurchaseDeletionSet.to_json
               v.successful_queued_purchase_deletions )
       ])

let of_json j =
  { successful_queued_purchase_deletions =
      SuccessfulQueuedPurchaseDeletionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "successfulQueuedPurchaseDeletionSet"))
  ; failed_queued_purchase_deletions =
      FailedQueuedPurchaseDeletionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "failedQueuedPurchaseDeletionSet"))
  }
