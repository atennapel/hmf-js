const {
  Abs, Var, App, showTerm, Ann,
} = require('./terms');
const { infer } = require('./inferRigid');
const {
  showType,
  TForall,
  TFun,
  TVar,
  TCon,
  TApp,
  Annot,
} = require('./types');

const tid = TForall([0], TFun(TVar(0), TVar(0)));
const tlist = TCon('List');

const env = {
  id: tid,
  apply: TForall([0, 1], TFun(TFun(TVar(0), TVar(1)), TFun(TVar(0), TVar(1)))),
  auto: TFun(tid, tid),
  ids: TApp(tlist, tid),
  map: TForall([0, 1], TFun(TFun(TVar(0), TVar(1)), TFun(TApp(tlist, TVar(0)), TApp(tlist, TVar(1))))),
  singleton: TForall([0], TFun(TVar(0), TApp(tlist, TVar(0)))),
  head: TForall([0], TFun(TApp(tlist, TVar(0)), TVar(0))),
};

const term = Ann(App(Var('singleton'), Var('id')), Annot([], TForall([], TApp(tlist, tid))));
console.log(showTerm(term));
const ty = infer(env, term);
console.log(showType(ty));

