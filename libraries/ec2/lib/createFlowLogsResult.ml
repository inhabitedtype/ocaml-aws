open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; flow_log_ids : ValueStringList.t
  ; unsuccessful : UnsuccessfulItemSet.t
  }

let make ?client_token ?(flow_log_ids = []) ?(unsuccessful = []) () =
  { client_token; flow_log_ids; unsuccessful }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; flow_log_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "flowLogIdSet" xml) ValueStringList.parse)
    ; unsuccessful =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "unsuccessful" xml)
             UnsuccessfulItemSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Unsuccessful", UnsuccessfulItemSet.to_query v.unsuccessful))
       ; Some (Aws.Query.Pair ("FlowLogIdSet", ValueStringList.to_query v.flow_log_ids))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("unsuccessful", UnsuccessfulItemSet.to_json v.unsuccessful)
       ; Some ("flowLogIdSet", ValueStringList.to_json v.flow_log_ids)
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; flow_log_ids =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "flowLogIdSet"))
  ; unsuccessful =
      UnsuccessfulItemSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "unsuccessful"))
  }
