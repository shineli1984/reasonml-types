/*
   recursive type
   right associate
   foldable
 */
type list('a) =
  | Empty
  | Cons('a, list('a));

let (@:) = (h, t) => Cons(h, t);

let l = 1 @: 2 @: 3 @: Empty;

type tree('a) =
  | Leaf
  | Node('a, tree('a), tree('a));

let (@-<) = (v, (l, r)) => Node(v, l, r);

let t = 1 @-< (2 @-< (Leaf, Leaf), Leaf);

let rec sum = (tree, acc) =>
  switch (tree) {
  | Leaf => acc
  | Node(v, l, r) => v + sum(l, acc) + sum(r, acc)
  };

module type Foldable = {
  type t('a);
  let fold: (('b, 'a) => 'b, 'b, t('a)) => 'b;
};

module TreeFoldable: Foldable with type t('a) = tree('a) = {
  type t('a) = tree('a);
  let rec fold = (f, i, t) =>
    switch (t) {
    | Leaf => i
    | Node(v, l, r) => fold(f, fold(f, f(i, v), l), r)
    };
};

let r = TreeFoldable.fold((acc, v) => acc + v, 0, t);

Js.log(r);
