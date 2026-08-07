open Aws.BaseTypes

type t =
  { id : String.t option
  ; caller_reference : String.t option
  ; name_servers : DelegationSetNameServers.t
  }

let make ?id ?caller_reference ~name_servers () = { id; caller_reference; name_servers }

let parse xml =
  Some
    { id = Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse
    ; caller_reference =
        Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse
    ; name_servers =
        Aws.Xml.required
          "NameServers"
          (Aws.Util.option_bind
             (Aws.Xml.member "NameServers" xml)
             DelegationSetNameServers.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("NameServers.member", DelegationSetNameServers.to_query v.name_servers))
       ; Aws.Util.option_map v.caller_reference (fun f ->
             Aws.Query.Pair ("CallerReference", String.to_query f))
       ; Aws.Util.option_map v.id (fun f -> Aws.Query.Pair ("Id", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("NameServers", DelegationSetNameServers.to_json v.name_servers)
       ; Aws.Util.option_map v.caller_reference (fun f ->
             "CallerReference", String.to_json f)
       ; Aws.Util.option_map v.id (fun f -> "Id", String.to_json f)
       ])

let of_json j =
  { id = Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json
  ; caller_reference =
      Aws.Util.option_map (Aws.Json.lookup j "CallerReference") String.of_json
  ; name_servers =
      DelegationSetNameServers.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NameServers"))
  }
