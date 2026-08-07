type t = { references : PerformanceFactorReferenceSet.t }

let make ?(references = []) () = { references }

let parse xml =
  Some
    { references =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "referenceSet" xml)
             PerformanceFactorReferenceSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ReferenceSet", PerformanceFactorReferenceSet.to_query v.references))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("referenceSet", PerformanceFactorReferenceSet.to_json v.references) ])

let of_json j =
  { references =
      PerformanceFactorReferenceSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "referenceSet"))
  }
