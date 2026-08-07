open Aws.BaseTypes

type t =
  { property_path : String.t
  ; expected_value : String.t
  ; actual_value : String.t
  ; difference_type : DifferenceType.t
  }

let make ~property_path ~expected_value ~actual_value ~difference_type () =
  { property_path; expected_value; actual_value; difference_type }

let parse xml =
  Some
    { property_path =
        Aws.Xml.required
          "PropertyPath"
          (Aws.Util.option_bind (Aws.Xml.member "PropertyPath" xml) String.parse)
    ; expected_value =
        Aws.Xml.required
          "ExpectedValue"
          (Aws.Util.option_bind (Aws.Xml.member "ExpectedValue" xml) String.parse)
    ; actual_value =
        Aws.Xml.required
          "ActualValue"
          (Aws.Util.option_bind (Aws.Xml.member "ActualValue" xml) String.parse)
    ; difference_type =
        Aws.Xml.required
          "DifferenceType"
          (Aws.Util.option_bind
             (Aws.Xml.member "DifferenceType" xml)
             DifferenceType.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("DifferenceType", DifferenceType.to_query v.difference_type))
       ; Some (Aws.Query.Pair ("ActualValue", String.to_query v.actual_value))
       ; Some (Aws.Query.Pair ("ExpectedValue", String.to_query v.expected_value))
       ; Some (Aws.Query.Pair ("PropertyPath", String.to_query v.property_path))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DifferenceType", DifferenceType.to_json v.difference_type)
       ; Some ("ActualValue", String.to_json v.actual_value)
       ; Some ("ExpectedValue", String.to_json v.expected_value)
       ; Some ("PropertyPath", String.to_json v.property_path)
       ])

let of_json j =
  { property_path =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PropertyPath"))
  ; expected_value =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ExpectedValue"))
  ; actual_value =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ActualValue"))
  ; difference_type =
      DifferenceType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DifferenceType"))
  }
