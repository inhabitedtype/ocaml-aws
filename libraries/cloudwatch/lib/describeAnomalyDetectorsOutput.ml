open Aws.BaseTypes

type t =
  { anomaly_detectors : AnomalyDetectors.t
  ; next_token : String.t option
  }

let make ?(anomaly_detectors = []) ?next_token () = { anomaly_detectors; next_token }

let parse xml =
  Some
    { anomaly_detectors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AnomalyDetectors" xml)
             AnomalyDetectors.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AnomalyDetectors.member", AnomalyDetectors.to_query v.anomaly_detectors))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("AnomalyDetectors", AnomalyDetectors.to_json v.anomaly_detectors)
       ])

let of_json j =
  { anomaly_detectors =
      AnomalyDetectors.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AnomalyDetectors"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
