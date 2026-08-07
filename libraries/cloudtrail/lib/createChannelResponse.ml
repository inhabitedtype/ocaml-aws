open Aws.BaseTypes

type t =
  { channel_arn : String.t option
  ; name : String.t option
  ; source : String.t option
  ; destinations : Destinations.t
  ; tags : TagsList.t
  }

let make ?channel_arn ?name ?source ?(destinations = []) ?(tags = []) () =
  { channel_arn; name; source; destinations; tags }

let parse xml =
  Some
    { channel_arn = Aws.Util.option_bind (Aws.Xml.member "ChannelArn" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; source = Aws.Util.option_bind (Aws.Xml.member "Source" xml) String.parse
    ; destinations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Destinations" xml) Destinations.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagsList.to_query v.tags))
       ; Some
           (Aws.Query.Pair ("Destinations.member", Destinations.to_query v.destinations))
       ; Aws.Util.option_map v.source (fun f ->
             Aws.Query.Pair ("Source", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.channel_arn (fun f ->
             Aws.Query.Pair ("ChannelArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagsList.to_json v.tags)
       ; Some ("Destinations", Destinations.to_json v.destinations)
       ; Aws.Util.option_map v.source (fun f -> "Source", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.channel_arn (fun f -> "ChannelArn", String.to_json f)
       ])

let of_json j =
  { channel_arn = Aws.Util.option_map (Aws.Json.lookup j "ChannelArn") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; source = Aws.Util.option_map (Aws.Json.lookup j "Source") String.of_json
  ; destinations =
      Destinations.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Destinations"))
  ; tags = TagsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
