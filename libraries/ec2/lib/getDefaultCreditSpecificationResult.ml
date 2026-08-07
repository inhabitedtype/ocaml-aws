type t =
  { instance_family_credit_specification : InstanceFamilyCreditSpecification.t option }

let make ?instance_family_credit_specification () =
  { instance_family_credit_specification }

let parse xml =
  Some
    { instance_family_credit_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceFamilyCreditSpecification" xml)
          InstanceFamilyCreditSpecification.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_family_credit_specification (fun f ->
             Aws.Query.Pair
               ( "InstanceFamilyCreditSpecification"
               , InstanceFamilyCreditSpecification.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_family_credit_specification (fun f ->
             ( "instanceFamilyCreditSpecification"
             , InstanceFamilyCreditSpecification.to_json f ))
       ])

let of_json j =
  { instance_family_credit_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceFamilyCreditSpecification")
        InstanceFamilyCreditSpecification.of_json
  }
