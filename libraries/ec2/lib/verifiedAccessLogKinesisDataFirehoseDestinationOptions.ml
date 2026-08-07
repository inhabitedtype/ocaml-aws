open Aws.BaseTypes

type t =
  { enabled : Boolean.t
  ; delivery_stream : String.t option
  }

let make ~enabled ?delivery_stream () = { enabled; delivery_stream }

let parse xml =
  Some
    { enabled =
        Aws.Xml.required
          "Enabled"
          (Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse)
    ; delivery_stream =
        Aws.Util.option_bind (Aws.Xml.member "DeliveryStream" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delivery_stream (fun f ->
             Aws.Query.Pair ("DeliveryStream", String.to_query f))
       ; Some (Aws.Query.Pair ("Enabled", Boolean.to_query v.enabled))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delivery_stream (fun f ->
             "DeliveryStream", String.to_json f)
       ; Some ("Enabled", Boolean.to_json v.enabled)
       ])

let of_json j =
  { enabled = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Enabled"))
  ; delivery_stream =
      Aws.Util.option_map (Aws.Json.lookup j "DeliveryStream") String.of_json
  }
