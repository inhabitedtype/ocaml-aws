open Aws.BaseTypes

type t = { integration_identifier : String.t }

let make ~integration_identifier () = { integration_identifier }

let parse xml =
  Some
    { integration_identifier =
        Aws.Xml.required
          "IntegrationIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "IntegrationIdentifier" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("IntegrationIdentifier", String.to_query v.integration_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("IntegrationIdentifier", String.to_json v.integration_identifier) ])

let of_json j =
  { integration_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IntegrationIdentifier"))
  }
