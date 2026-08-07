open Aws.BaseTypes

type t =
  { channels : Channels.t
  ; next_token : String.t option
  }

let make ?(channels = []) ?next_token () = { channels; next_token }

let parse xml =
  Some
    { channels =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Channels" xml) Channels.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Channels.member", Channels.to_query v.channels))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Channels", Channels.to_json v.channels)
       ])

let of_json j =
  { channels = Channels.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Channels"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
