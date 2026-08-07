type t = { asn_association : AsnAssociation.t option }

let make ?asn_association () = { asn_association }

let parse xml =
  Some
    { asn_association =
        Aws.Util.option_bind (Aws.Xml.member "asnAssociation" xml) AsnAssociation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.asn_association (fun f ->
             Aws.Query.Pair ("AsnAssociation", AsnAssociation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.asn_association (fun f ->
             "asnAssociation", AsnAssociation.to_json f)
       ])

let of_json j =
  { asn_association =
      Aws.Util.option_map (Aws.Json.lookup j "asnAssociation") AsnAssociation.of_json
  }
