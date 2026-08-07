open Aws.BaseTypes

type t =
  { messages : ErrorMessages.t
  ; message : String.t option
  }

let make ?(messages = []) ?message () = { messages; message }

let parse xml =
  Some
    { messages =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "messages" xml) ErrorMessages.parse)
    ; message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message (fun f ->
             Aws.Query.Pair ("message", String.to_query f))
       ; Some (Aws.Query.Pair ("messages.member", ErrorMessages.to_query v.messages))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f)
       ; Some ("messages", ErrorMessages.to_json v.messages)
       ])

let of_json j =
  { messages =
      ErrorMessages.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "messages"))
  ; message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json
  }
