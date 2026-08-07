type t = { instances : Instances.t }

let make ?(instances = []) () = { instances }

let parse xml =
  Some
    { instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Instances" xml) Instances.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Instances.member", Instances.to_query v.instances)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Instances", Instances.to_json v.instances) ])

let of_json j =
  { instances = Instances.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
  }
