open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; reserved_instances_offerings : ReservedInstancesOfferingList.t
  }

let make ?next_token ?(reserved_instances_offerings = []) () =
  { next_token; reserved_instances_offerings }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; reserved_instances_offerings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "reservedInstancesOfferingsSet" xml)
             ReservedInstancesOfferingList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ReservedInstancesOfferingsSet"
              , ReservedInstancesOfferingList.to_query v.reserved_instances_offerings ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "reservedInstancesOfferingsSet"
           , ReservedInstancesOfferingList.to_json v.reserved_instances_offerings )
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; reserved_instances_offerings =
      ReservedInstancesOfferingList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "reservedInstancesOfferingsSet"))
  }
