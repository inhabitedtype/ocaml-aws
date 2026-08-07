open Aws.BaseTypes

type t =
  { connection_id : String.t option
  ; previous_status : ClientVpnConnectionStatus.t option
  ; current_status : ClientVpnConnectionStatus.t option
  }

let make ?connection_id ?previous_status ?current_status () =
  { connection_id; previous_status; current_status }

let parse xml =
  Some
    { connection_id =
        Aws.Util.option_bind (Aws.Xml.member "connectionId" xml) String.parse
    ; previous_status =
        Aws.Util.option_bind
          (Aws.Xml.member "previousStatus" xml)
          ClientVpnConnectionStatus.parse
    ; current_status =
        Aws.Util.option_bind
          (Aws.Xml.member "currentStatus" xml)
          ClientVpnConnectionStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.current_status (fun f ->
             Aws.Query.Pair ("CurrentStatus", ClientVpnConnectionStatus.to_query f))
       ; Aws.Util.option_map v.previous_status (fun f ->
             Aws.Query.Pair ("PreviousStatus", ClientVpnConnectionStatus.to_query f))
       ; Aws.Util.option_map v.connection_id (fun f ->
             Aws.Query.Pair ("ConnectionId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.current_status (fun f ->
             "currentStatus", ClientVpnConnectionStatus.to_json f)
       ; Aws.Util.option_map v.previous_status (fun f ->
             "previousStatus", ClientVpnConnectionStatus.to_json f)
       ; Aws.Util.option_map v.connection_id (fun f -> "connectionId", String.to_json f)
       ])

let of_json j =
  { connection_id = Aws.Util.option_map (Aws.Json.lookup j "connectionId") String.of_json
  ; previous_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "previousStatus")
        ClientVpnConnectionStatus.of_json
  ; current_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "currentStatus")
        ClientVpnConnectionStatus.of_json
  }
