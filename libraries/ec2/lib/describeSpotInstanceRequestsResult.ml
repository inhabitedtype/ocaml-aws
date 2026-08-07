open Aws.BaseTypes

type t =
  { spot_instance_requests : SpotInstanceRequestList.t
  ; next_token : String.t option
  }

let make ?(spot_instance_requests = []) ?next_token () =
  { spot_instance_requests; next_token }

let parse xml =
  Some
    { spot_instance_requests =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "spotInstanceRequestSet" xml)
             SpotInstanceRequestList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SpotInstanceRequestSet"
              , SpotInstanceRequestList.to_query v.spot_instance_requests ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "spotInstanceRequestSet"
           , SpotInstanceRequestList.to_json v.spot_instance_requests )
       ])

let of_json j =
  { spot_instance_requests =
      SpotInstanceRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "spotInstanceRequestSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
