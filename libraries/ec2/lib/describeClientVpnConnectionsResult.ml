open Aws.BaseTypes

type t =
  { connections : ClientVpnConnectionSet.t
  ; next_token : String.t option
  }

let make ?(connections = []) ?next_token () = { connections; next_token }

let parse xml =
  Some
    { connections =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "connections" xml)
             ClientVpnConnectionSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair ("Connections", ClientVpnConnectionSet.to_query v.connections))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("connections", ClientVpnConnectionSet.to_json v.connections)
       ])

let of_json j =
  { connections =
      ClientVpnConnectionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "connections"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
