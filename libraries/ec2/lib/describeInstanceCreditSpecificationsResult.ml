open Aws.BaseTypes

type t =
  { instance_credit_specifications : InstanceCreditSpecificationList.t
  ; next_token : String.t option
  }

let make ?(instance_credit_specifications = []) ?next_token () =
  { instance_credit_specifications; next_token }

let parse xml =
  Some
    { instance_credit_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceCreditSpecificationSet" xml)
             InstanceCreditSpecificationList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InstanceCreditSpecificationSet"
              , InstanceCreditSpecificationList.to_query v.instance_credit_specifications
              ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "instanceCreditSpecificationSet"
           , InstanceCreditSpecificationList.to_json v.instance_credit_specifications )
       ])

let of_json j =
  { instance_credit_specifications =
      InstanceCreditSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceCreditSpecificationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
