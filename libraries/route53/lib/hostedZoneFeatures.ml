type t =
  { accelerated_recovery_status : AcceleratedRecoveryStatus.t option
  ; failure_reasons : HostedZoneFailureReasons.t option
  }

let make ?accelerated_recovery_status ?failure_reasons () =
  { accelerated_recovery_status; failure_reasons }

let parse xml =
  Some
    { accelerated_recovery_status =
        Aws.Util.option_bind
          (Aws.Xml.member "AcceleratedRecoveryStatus" xml)
          AcceleratedRecoveryStatus.parse
    ; failure_reasons =
        Aws.Util.option_bind
          (Aws.Xml.member "FailureReasons" xml)
          HostedZoneFailureReasons.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failure_reasons (fun f ->
             Aws.Query.Pair ("FailureReasons", HostedZoneFailureReasons.to_query f))
       ; Aws.Util.option_map v.accelerated_recovery_status (fun f ->
             Aws.Query.Pair
               ("AcceleratedRecoveryStatus", AcceleratedRecoveryStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failure_reasons (fun f ->
             "FailureReasons", HostedZoneFailureReasons.to_json f)
       ; Aws.Util.option_map v.accelerated_recovery_status (fun f ->
             "AcceleratedRecoveryStatus", AcceleratedRecoveryStatus.to_json f)
       ])

let of_json j =
  { accelerated_recovery_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "AcceleratedRecoveryStatus")
        AcceleratedRecoveryStatus.of_json
  ; failure_reasons =
      Aws.Util.option_map
        (Aws.Json.lookup j "FailureReasons")
        HostedZoneFailureReasons.of_json
  }
