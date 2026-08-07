type t = unit

let make () = ()
let parse xml = Some ()
let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
let to_json v = `Assoc (Aws.Util.list_filter_opt [])
let of_json j = ()
