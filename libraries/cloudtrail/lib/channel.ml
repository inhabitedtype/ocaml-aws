open Aws.BaseTypes

type t =
  { channel_arn : String.t option
  ; name : String.t option
  }

let make ?channel_arn ?name () = { channel_arn; name }

let parse xml =
  Some
    { channel_arn = Aws.Util.option_bind (Aws.Xml.member "ChannelArn" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.channel_arn (fun f ->
             Aws.Query.Pair ("ChannelArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.channel_arn (fun f -> "ChannelArn", String.to_json f)
       ])

let of_json j =
  { channel_arn = Aws.Util.option_map (Aws.Json.lookup j "ChannelArn") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  }
