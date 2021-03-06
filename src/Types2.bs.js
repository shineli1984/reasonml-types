// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

function head(list) {
  if (list) {
    return /* Just */[list[0]];
  } else {
    return /* Nothing */0;
  }
}

function lte2(a) {
  return a >= 2;
}

function lte2$prime(ma) {
  if (ma) {
    return /* Just */[ma[0] >= 2];
  } else {
    return /* Nothing */0;
  }
}

var headLte2 = lte2$prime(head(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]));

function fmap(f, m) {
  if (m) {
    return /* Just */[Curry._1(f, m[0])];
  } else {
    return /* Nothing */0;
  }
}

var MaybeF = /* module */[/* fmap */fmap];

var headLte2$prime = fmap(lte2, head(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]));

exports.head = head;
exports.lte2 = lte2;
exports.lte2$prime = lte2$prime;
exports.headLte2 = headLte2;
exports.MaybeF = MaybeF;
exports.headLte2$prime = headLte2$prime;
/* headLte2 Not a pure module */
