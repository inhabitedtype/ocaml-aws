open Aws.BaseTypes

type t =
  { network_interface : NetworkInterface.t option
  ; client_token : String.t option
  }

let make ?network_interface ?client_token () = { network_interface; client_token }

let parse xml =
  Some
    { network_interface =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInterface" xml)
          NetworkInterface.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.network_interface (fun f ->
             Aws.Query.Pair ("NetworkInterface", NetworkInterface.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.network_interface (fun f ->
             "networkInterface", NetworkInterface.to_json f)
       ])

let of_json j =
  { network_interface =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterface") NetworkInterface.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  }
