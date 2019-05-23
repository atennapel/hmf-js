const {
  abs, Var, app, showTerm, Ann,
} = require('./terms');
const { infer } = require('./inferNaryProp');
const {
  showType,
  TForall,
  TVar,
  TCon,
  Annot,
  tfun,
  tapp,
} = require('./types');

const tv = TVar;
const tid = TForall([0], tfun(tv(0), tv(0)));
const tlist = TCon('List');

const env = {
  id: tid,
  apply: TForall([0, 1], tfun(tfun(tv(0), tv(1)), tv(0), tv(1))),
  auto: tfun(tid, tid),
  ids: tapp(tlist, tid),
  map: TForall([0, 1], tfun(tfun(tv(0), tv(1)), tapp(tlist, tv(0)), tapp(tlist, tv(1)))),
  singleton: TForall([0], tfun(tv(0), tapp(tlist, tv(0)))),
  head: TForall([0], tfun(tapp(tlist, tv(0)), tv(0))),
};

const v = Var;

const term = Ann(app(v('singleton'), v('id')), Annot([], tapp(tlist, tid)));
// const term = App(Var('singleton'), Var('id'));
// const term = abs(['x'], v('x'));
// const term = v('auto');
// const term = app(v('auto'), v('id'));
// const term = app(v('apply'), v('auto'), v('id'));
// const term = app(v('map'), v('auto'), v('ids'));
console.log(showTerm(term));
const ty = infer(env, term);
console.log(showType(ty));

