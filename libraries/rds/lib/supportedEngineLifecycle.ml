open Aws.BaseTypes

type t =
  { lifecycle_support_name : LifecycleSupportName.t
  ; lifecycle_support_start_date : DateTime.t
  ; lifecycle_support_end_date : DateTime.t
  }

let make
    ~lifecycle_support_name
    ~lifecycle_support_start_date
    ~lifecycle_support_end_date
    () =
  { lifecycle_support_name; lifecycle_support_start_date; lifecycle_support_end_date }

let parse xml =
  Some
    { lifecycle_support_name =
        Aws.Xml.required
          "LifecycleSupportName"
          (Aws.Util.option_bind
             (Aws.Xml.member "LifecycleSupportName" xml)
             LifecycleSupportName.parse)
    ; lifecycle_support_start_date =
        Aws.Xml.required
          "LifecycleSupportStartDate"
          (Aws.Util.option_bind
             (Aws.Xml.member "LifecycleSupportStartDate" xml)
             DateTime.parse)
    ; lifecycle_support_end_date =
        Aws.Xml.required
          "LifecycleSupportEndDate"
          (Aws.Util.option_bind
             (Aws.Xml.member "LifecycleSupportEndDate" xml)
             DateTime.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("LifecycleSupportEndDate", DateTime.to_query v.lifecycle_support_end_date))
       ; Some
           (Aws.Query.Pair
              ( "LifecycleSupportStartDate"
              , DateTime.to_query v.lifecycle_support_start_date ))
       ; Some
           (Aws.Query.Pair
              ( "LifecycleSupportName"
              , LifecycleSupportName.to_query v.lifecycle_support_name ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LifecycleSupportEndDate", DateTime.to_json v.lifecycle_support_end_date)
       ; Some
           ("LifecycleSupportStartDate", DateTime.to_json v.lifecycle_support_start_date)
       ; Some
           ("LifecycleSupportName", LifecycleSupportName.to_json v.lifecycle_support_name)
       ])

let of_json j =
  { lifecycle_support_name =
      LifecycleSupportName.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleSupportName"))
  ; lifecycle_support_start_date =
      DateTime.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleSupportStartDate"))
  ; lifecycle_support_end_date =
      DateTime.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleSupportEndDate"))
  }
