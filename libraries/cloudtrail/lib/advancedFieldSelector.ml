open Aws.BaseTypes

type t =
  { field : String.t
  ; equals : Operator.t
  ; starts_with : Operator.t
  ; ends_with : Operator.t
  ; not_equals : Operator.t
  ; not_starts_with : Operator.t
  ; not_ends_with : Operator.t
  }

let make
    ~field
    ?(equals = [])
    ?(starts_with = [])
    ?(ends_with = [])
    ?(not_equals = [])
    ?(not_starts_with = [])
    ?(not_ends_with = [])
    () =
  { field; equals; starts_with; ends_with; not_equals; not_starts_with; not_ends_with }

let parse xml =
  Some
    { field =
        Aws.Xml.required
          "Field"
          (Aws.Util.option_bind (Aws.Xml.member "Field" xml) String.parse)
    ; equals =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Equals" xml) Operator.parse)
    ; starts_with =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "StartsWith" xml) Operator.parse)
    ; ends_with =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "EndsWith" xml) Operator.parse)
    ; not_equals =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "NotEquals" xml) Operator.parse)
    ; not_starts_with =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "NotStartsWith" xml) Operator.parse)
    ; not_ends_with =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "NotEndsWith" xml) Operator.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("NotEndsWith.member", Operator.to_query v.not_ends_with))
       ; Some
           (Aws.Query.Pair ("NotStartsWith.member", Operator.to_query v.not_starts_with))
       ; Some (Aws.Query.Pair ("NotEquals.member", Operator.to_query v.not_equals))
       ; Some (Aws.Query.Pair ("EndsWith.member", Operator.to_query v.ends_with))
       ; Some (Aws.Query.Pair ("StartsWith.member", Operator.to_query v.starts_with))
       ; Some (Aws.Query.Pair ("Equals.member", Operator.to_query v.equals))
       ; Some (Aws.Query.Pair ("Field", String.to_query v.field))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("NotEndsWith", Operator.to_json v.not_ends_with)
       ; Some ("NotStartsWith", Operator.to_json v.not_starts_with)
       ; Some ("NotEquals", Operator.to_json v.not_equals)
       ; Some ("EndsWith", Operator.to_json v.ends_with)
       ; Some ("StartsWith", Operator.to_json v.starts_with)
       ; Some ("Equals", Operator.to_json v.equals)
       ; Some ("Field", String.to_json v.field)
       ])

let of_json j =
  { field = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Field"))
  ; equals = Operator.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Equals"))
  ; starts_with =
      Operator.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartsWith"))
  ; ends_with = Operator.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EndsWith"))
  ; not_equals = Operator.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NotEquals"))
  ; not_starts_with =
      Operator.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NotStartsWith"))
  ; not_ends_with =
      Operator.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NotEndsWith"))
  }
