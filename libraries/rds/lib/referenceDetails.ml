type t = { scalar_reference_details : ScalarReferenceDetails.t option }

let make ?scalar_reference_details () = { scalar_reference_details }

let parse xml =
  Some
    { scalar_reference_details =
        Aws.Util.option_bind
          (Aws.Xml.member "ScalarReferenceDetails" xml)
          ScalarReferenceDetails.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scalar_reference_details (fun f ->
             Aws.Query.Pair ("ScalarReferenceDetails", ScalarReferenceDetails.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scalar_reference_details (fun f ->
             "ScalarReferenceDetails", ScalarReferenceDetails.to_json f)
       ])

let of_json j =
  { scalar_reference_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "ScalarReferenceDetails")
        ScalarReferenceDetails.of_json
  }
