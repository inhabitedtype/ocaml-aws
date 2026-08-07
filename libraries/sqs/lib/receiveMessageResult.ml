type t = { messages : MessageList.t }

let make ?(messages = []) () = { messages }
let parse xml = Some { messages = Aws.Util.of_option [] (MessageList.parse xml) }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Messages.member", MessageList.to_query v.messages)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Messages", MessageList.to_json v.messages) ])

let of_json j =
  { messages = MessageList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Messages"))
  }
