const {
  Var,
  abs,
  Abs,
  app,
  Ann,
  showTerm,
} = require('./terms');
const {
  showType,
  TForall,
  TVar,
  tapp,
  tfun,
  TCon,
} = require('./types');
const { infer } = require('./infer');

const tv = TVar;
const tid = TForall([0], tfun(tv(0), tv(0)));
const tBool = TCon('Bool');
const tInt = TCon('Int');
const tList = TCon('List');
const tPair = TCon('Pair');
const tST = TCon('ST');

const v = Var;

const env = {
  True: tBool,
  False: tBool,
  zero: tInt,
  single: TForall([0], tfun(tv(0), tapp(tList, tv(0)))),
  id: tid,
  head: TForall([0], tfun(tapp(tList, tv(0)), tv(0))),
  tail: TForall([0], tfun(tapp(tList, tv(0)), tapp(tList, tv(0)))),
  Nil: TForall([0], tapp(tList, tv(0))),
  Cons: TForall([0], tfun(tv(0), tapp(tList, tv(0)), tapp(tList, tv(0)))),
  snoc: TForall([0], tfun(tapp(tList, tv(0)), tv(0), tapp(tList, tv(0)))),
  append: TForall([0], tfun(tapp(tList, tv(0)), tapp(tList, tv(0)), tapp(tList, tv(0)))),
  length: TForall([0], tfun(tapp(tList, tv(0)), tInt)),
  inc: tfun(tInt, tInt),
  choose: TForall([0], tfun(tv(0), tv(0), tv(0))),
  poly: tfun(tid, tapp(tPair, tInt, tBool)),
  auto: tfun(tid, tid),
  auto2: TForall([0], tfun(tid, tv(0), tv(0))),
  ids: tapp(tList, tid),
  map: TForall([0, 1], tfun(tfun(tv(0), tv(1)), tapp(tList, tv(0)), tapp(tList, tv(1)))),
  app: TForall([0, 1], tfun(tfun(tv(0), tv(1)), tv(0), tv(1))),
  revapp: TForall([0, 1], tfun(tv(0), tfun(tv(0), tv(1)), tv(1))),
  flip: TForall([0, 1, 2], tfun(tfun(tv(0), tv(1), tv(2)), tv(1), tv(0), tv(2))),
  runST: TForall([0], tfun(TForall([1], tapp(tST, tv(1), tv(0))), tv(0))),
  argST: TForall([0], tapp(tST, tv(0), tInt)),
  f: TForall([0], tfun(tfun(tv(0), tv(0)), tapp(tList, tv(0)), tv(0))),
  g: TForall([0], tfun(tapp(tList, tv(0)), tapp(tList, tv(0))), tv(0)),
  h: tfun(tInt, tid),
  k: TForall([0], tfun(tv(0), tapp(tList, tv(0)), tv(0))),
  lst: tapp(tList, TForall([0], tfun(tInt, tv(0), tv(0)))),
  r: tfun(TForall([0], tfun(tv(0), TForall([1], tfun(tv(1), tv(1))))), tInt),
};

const term = app(v('Cons'), v('id'), v('ids'));
// const term = Ann(abs(['x'], v('x')), tid);
console.log(showTerm(term));
try {
  const ty = infer(env, term);
  console.log(showType(ty));
} catch (err) {
  console.log(err);
}

/**
 * Type annotation should succeed:
 *  choose (id : tid -> tid) auto2
 *
 * Also fails in HMF:
 *  Cons id ids
 *  Cons (\x -> x) ids
 *  g (single id) ids
 *  k (\x -> h x) lst
 *  r (\x y -> y)
 *
 * Fixed by n-ary:
 *  revapp id poly
 *  Cons id ids
 */
