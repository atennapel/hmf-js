const {
  Abs, Var, App, showTerm
} = require('./terms');
const { infer } = require('./inferBasic');
const {
  showType,
  TForall,
  TFun,
  TVar,
  TCon,
  TApp,
} = require('./types');

const tid = TForall([0], TFun(TVar(0), TVar(0)));
const tlist = TCon('List');

const env = {
  id: tid,
  apply: TForall([0, 1], TFun(TFun(TVar(0), TVar(1)), TFun(TVar(0), TVar(1)))),
  auto: TFun(tid, tid),
  ids: TApp(tlist, tid),
  map: TForall([0, 1], TFun(TFun(TVar(0), TVar(1)), TFun(TApp(tlist, TVar(0)), TApp(tlist, TVar(1))))),
};

const term = App(App(Var('map'), Var('auto')), Var('ids'));
console.log(showTerm(term));
const ty = infer(env, term);
console.log(showType(ty));

