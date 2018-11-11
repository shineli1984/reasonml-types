/*
   variants with type parameters, type level function
   naive way of using it
   functor
 */

/* redefine type option('a) = None | Some('a) */
type maybe('a) =
  | Nothing
  | Just('a);

let head = list =>
  switch (list) {
  | [x, ..._] => Just(x)
  | _ => Nothing
  };

let lte2 = a => a >= 2;

let lte2' = ma =>
  switch (ma) {
  | Nothing => Nothing
  | Just(a) => Just(a >= 2)
  };

let headLte2 = [1, 2] |> head |> lte2';

module type Functor = {
  type t('a);
  let fmap: ('a => 'b, t('a)) => t('b);
};

module MaybeF: Functor with type t('a) = maybe('a) = {
  type t('a) = maybe('a);
  let fmap = (f, m) =>
    switch (m) {
    | Nothing => Nothing
    | Just(a) => Just(f(a))
    };
};

let headLte2' = [1, 2] |> head |> MaybeF.fmap(lte2);
