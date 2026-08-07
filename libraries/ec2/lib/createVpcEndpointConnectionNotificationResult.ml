open Aws.BaseTypes

type t =
  { connection_notification : ConnectionNotification.t option
  ; client_token : String.t option
  }

let make ?connection_notification ?client_token () =
  { connection_notification; client_token }

let parse xml =
  Some
    { connection_notification =
        Aws.Util.option_bind
          (Aws.Xml.member "connectionNotification" xml)
          ConnectionNotification.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.connection_notification (fun f ->
             Aws.Query.Pair ("ConnectionNotification", ConnectionNotification.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.connection_notification (fun f ->
             "connectionNotification", ConnectionNotification.to_json f)
       ])

let of_json j =
  { connection_notification =
      Aws.Util.option_map
        (Aws.Json.lookup j "connectionNotification")
        ConnectionNotification.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  }
