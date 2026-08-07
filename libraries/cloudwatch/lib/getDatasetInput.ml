open Aws.BaseTypes

type t = { dataset_identifier : String.t }

let make ~dataset_identifier () = { dataset_identifier }

let parse xml =
  Some
    { dataset_identifier =
        Aws.Xml.required
          "DatasetIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DatasetIdentifier" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("DatasetIdentifier", String.to_query v.dataset_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DatasetIdentifier", String.to_json v.dataset_identifier) ])

let of_json j =
  { dataset_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DatasetIdentifier"))
  }
