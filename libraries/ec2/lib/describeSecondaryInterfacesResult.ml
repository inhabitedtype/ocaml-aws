open Aws.BaseTypes

type t =
  { secondary_interfaces : SecondaryInterfaceList.t
  ; next_token : String.t option
  }

let make ?(secondary_interfaces = []) ?next_token () =
  { secondary_interfaces; next_token }

let parse xml =
  Some
    { secondary_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "secondaryInterfaceSet" xml)
             SecondaryInterfaceList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecondaryInterfaceSet"
              , SecondaryInterfaceList.to_query v.secondary_interfaces ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ("secondaryInterfaceSet", SecondaryInterfaceList.to_json v.secondary_interfaces)
       ])

let of_json j =
  { secondary_interfaces =
      SecondaryInterfaceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "secondaryInterfaceSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
