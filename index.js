const {
  Var,
  abs,
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
const tList = TCon('List');

const v = Var;

const env = {
  singleton: TForall([0], tfun(tv(0), tapp(tList, tv(0)))),
};

const term = Ann(abs(['x'], v('x')), tfun(tid, tid));
console.log(showTerm(term));
const ty = infer(env, term);
console.log(showType(ty));
