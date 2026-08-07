open Aws.BaseTypes

type t =
  { hosts : HostList.t
  ; next_token : String.t option
  }

let make ?(hosts = []) ?next_token () = { hosts; next_token }

let parse xml =
  Some
    { hosts =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "hostSet" xml) HostList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("HostSet", HostList.to_query v.hosts))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("hostSet", HostList.to_json v.hosts)
       ])

let of_json j =
  { hosts = HostList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "hostSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
