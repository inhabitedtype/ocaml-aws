open Aws.BaseTypes

type t =
  { bucket : String.t
  ; object_key : String.t
  ; name : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make ~bucket ~object_key ?name ?(tag_specifications = []) ?dry_run () =
  { bucket; object_key; name; tag_specifications; dry_run }

let parse xml =
  Some
    { bucket =
        Aws.Xml.required
          "Bucket"
          (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml) String.parse)
    ; object_key =
        Aws.Xml.required
          "ObjectKey"
          (Aws.Util.option_bind (Aws.Xml.member "ObjectKey" xml) String.parse)
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Some (Aws.Query.Pair ("ObjectKey", String.to_query v.object_key))
       ; Some (Aws.Query.Pair ("Bucket", String.to_query v.bucket))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Some ("ObjectKey", String.to_json v.object_key)
       ; Some ("Bucket", String.to_json v.bucket)
       ])

let of_json j =
  { bucket = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket"))
  ; object_key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ObjectKey"))
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
