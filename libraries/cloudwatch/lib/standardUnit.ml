open Aws.BaseTypes

type t =
  | Seconds
  | Microseconds
  | Milliseconds
  | Bytes
  | Kilobytes
  | Megabytes
  | Gigabytes
  | Terabytes
  | Bits
  | Kilobits
  | Megabits
  | Gigabits
  | Terabits
  | Percent
  | Count
  | Bytes_Second
  | Kilobytes_Second
  | Megabytes_Second
  | Gigabytes_Second
  | Terabytes_Second
  | Bits_Second
  | Kilobits_Second
  | Megabits_Second
  | Gigabits_Second
  | Terabits_Second
  | Count_Second
  | None

let str_to_t =
  [ "None", None
  ; "Count/Second", Count_Second
  ; "Terabits/Second", Terabits_Second
  ; "Gigabits/Second", Gigabits_Second
  ; "Megabits/Second", Megabits_Second
  ; "Kilobits/Second", Kilobits_Second
  ; "Bits/Second", Bits_Second
  ; "Terabytes/Second", Terabytes_Second
  ; "Gigabytes/Second", Gigabytes_Second
  ; "Megabytes/Second", Megabytes_Second
  ; "Kilobytes/Second", Kilobytes_Second
  ; "Bytes/Second", Bytes_Second
  ; "Count", Count
  ; "Percent", Percent
  ; "Terabits", Terabits
  ; "Gigabits", Gigabits
  ; "Megabits", Megabits
  ; "Kilobits", Kilobits
  ; "Bits", Bits
  ; "Terabytes", Terabytes
  ; "Gigabytes", Gigabytes
  ; "Megabytes", Megabytes
  ; "Kilobytes", Kilobytes
  ; "Bytes", Bytes
  ; "Milliseconds", Milliseconds
  ; "Microseconds", Microseconds
  ; "Seconds", Seconds
  ]

let t_to_str =
  [ None, "None"
  ; Count_Second, "Count/Second"
  ; Terabits_Second, "Terabits/Second"
  ; Gigabits_Second, "Gigabits/Second"
  ; Megabits_Second, "Megabits/Second"
  ; Kilobits_Second, "Kilobits/Second"
  ; Bits_Second, "Bits/Second"
  ; Terabytes_Second, "Terabytes/Second"
  ; Gigabytes_Second, "Gigabytes/Second"
  ; Megabytes_Second, "Megabytes/Second"
  ; Kilobytes_Second, "Kilobytes/Second"
  ; Bytes_Second, "Bytes/Second"
  ; Count, "Count"
  ; Percent, "Percent"
  ; Terabits, "Terabits"
  ; Gigabits, "Gigabits"
  ; Megabits, "Megabits"
  ; Kilobits, "Kilobits"
  ; Bits, "Bits"
  ; Terabytes, "Terabytes"
  ; Gigabytes, "Gigabytes"
  ; Megabytes, "Megabytes"
  ; Kilobytes, "Kilobytes"
  ; Bytes, "Bytes"
  ; Milliseconds, "Milliseconds"
  ; Microseconds, "Microseconds"
  ; Seconds, "Seconds"
  ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
