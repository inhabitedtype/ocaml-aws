open Aws.BaseTypes

type t = { delivery_stream : String.t option }

let make ?delivery_stream () = { delivery_stream }

let parse xml =
  Some
    { delivery_stream =
        Aws.Util.option_bind (Aws.Xml.member "DeliveryStream" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delivery_stream (fun f ->
             Aws.Query.Pair ("DeliveryStream", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delivery_stream (fun f ->
             "DeliveryStream", String.to_json f)
       ])

let of_json j =
  { delivery_stream =
      Aws.Util.option_map (Aws.Json.lookup j "DeliveryStream") String.of_json
  }
