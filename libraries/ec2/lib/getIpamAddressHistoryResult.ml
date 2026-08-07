open Aws.BaseTypes

type t =
  { history_records : IpamAddressHistoryRecordSet.t
  ; next_token : String.t option
  }

let make ?(history_records = []) ?next_token () = { history_records; next_token }

let parse xml =
  Some
    { history_records =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "historyRecordSet" xml)
             IpamAddressHistoryRecordSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("HistoryRecordSet", IpamAddressHistoryRecordSet.to_query v.history_records))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("historyRecordSet", IpamAddressHistoryRecordSet.to_json v.history_records)
       ])

let of_json j =
  { history_records =
      IpamAddressHistoryRecordSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "historyRecordSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
