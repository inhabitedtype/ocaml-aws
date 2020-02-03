module Corpus = Corpus

let rec suchThat (gen : 'a QCheck.Gen.t) (p : 'a -> bool) : ('a QCheck.Gen.t) =
  let open QCheck.Gen in
  gen >>= function mx -> match p mx with
                         | true ->  pure mx
                         | false -> suchThat gen p
