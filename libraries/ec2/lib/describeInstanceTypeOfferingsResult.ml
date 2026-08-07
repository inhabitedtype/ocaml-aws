open Aws.BaseTypes

type t =
  { instance_type_offerings : InstanceTypeOfferingsList.t
  ; next_token : String.t option
  }

let make ?(instance_type_offerings = []) ?next_token () =
  { instance_type_offerings; next_token }

let parse xml =
  Some
    { instance_type_offerings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceTypeOfferingSet" xml)
             InstanceTypeOfferingsList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InstanceTypeOfferingSet"
              , InstanceTypeOfferingsList.to_query v.instance_type_offerings ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "instanceTypeOfferingSet"
           , InstanceTypeOfferingsList.to_json v.instance_type_offerings )
       ])

let of_json j =
  { instance_type_offerings =
      InstanceTypeOfferingsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceTypeOfferingSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
