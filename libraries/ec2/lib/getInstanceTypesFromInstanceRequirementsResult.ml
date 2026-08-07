open Aws.BaseTypes

type t =
  { instance_types : InstanceTypeInfoFromInstanceRequirementsSet.t
  ; next_token : String.t option
  }

let make ?(instance_types = []) ?next_token () = { instance_types; next_token }

let parse xml =
  Some
    { instance_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceTypeSet" xml)
             InstanceTypeInfoFromInstanceRequirementsSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InstanceTypeSet"
              , InstanceTypeInfoFromInstanceRequirementsSet.to_query v.instance_types ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "instanceTypeSet"
           , InstanceTypeInfoFromInstanceRequirementsSet.to_json v.instance_types )
       ])

let of_json j =
  { instance_types =
      InstanceTypeInfoFromInstanceRequirementsSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceTypeSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
