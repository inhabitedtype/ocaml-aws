let drop_empty l = List.filter (fun p -> String.length (String.trim (fst p)) <> 0) l

let or_error v err =
  match v with
  | None -> `Error err
  | Some v -> `Ok v

let of_option default = function
  | None -> default
  | Some v -> v

let of_option_exn = function
  | Some v -> v
  | None -> failwith "Expected Some v, got None."

let rec list_find l key =
  match l with
  | [] -> None
  | (k, v) :: xs -> if k = key then Some v else list_find xs key

let rec list_filter_opt = function
  | [] -> []
  | Some v :: xs -> v :: list_filter_opt xs
  | None :: xs -> list_filter_opt xs

let option_bind o f =
  match o with
  | None -> None
  | Some v -> f v

let option_map v f =
  match v with
  | None -> None
  | Some v -> Some (f v)

let rec option_all = function
  | [] -> Some []
  | Some v :: xs -> option_bind (option_all xs) (fun rest -> Some (v :: rest))
  | None :: _ -> None

let string_starts_with prefix s =
  let open String in
  let len_s = length s and len_pre = length prefix in
  let rec aux i =
    if i = len_pre
    then true
    else if unsafe_get s i <> unsafe_get prefix i
    then false
    else aux (i + 1)
  in
  len_s >= len_pre && aux 0
