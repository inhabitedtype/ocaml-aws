type t = { failures : BatchFailures.t }

let make ?(failures = []) () = { failures }

let parse xml =
  Some
    { failures =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Failures" xml) BatchFailures.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Failures.member", BatchFailures.to_query v.failures)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("Failures", BatchFailures.to_json v.failures) ])

let of_json j =
  { failures =
      BatchFailures.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Failures"))
  }
