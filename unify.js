const { instantiate, skolemize } = require('./operations');
const { showType, prune, matchTFun, freshTMeta, TFun, freeSkolems, freshTSkol, substTVars, containsTMeta } = require('./types');
const { terr } = require('./util');

const matchfun = ty => {
  const rho = instantiate(ty);
  const m = matchTFun(prune(rho));
  if (m) return m;
  if (m.tag === 'TMeta') {
    const a = freshTMeta();
    const b = freshTMeta();
    m.type = TFun(a, b);
    return { left: a, right: b };
  }
  return terr(`applying non-function: ${showType(ty)}`);
};

const subsume = (a, b) => {
  const [sks, rho1] = skolemize(a);
  const rho2 = instantiate(b);
  unify(rho1, rho2);
  const sk1 = freeSkolems(prune(a));
  const sk2 = freeSkolems(prune(b));
  if (sks.some(i => sk1.indexOf(i) >= 0 || sk2.indexOf(i) >= 0))
    return terr(`type not polymorphic enough in subsume: ${showType(a)} <: ${showType(b)}`);
};

const unify = (a, b) => {
  if (a === b) return;
  if (a.tag === 'TCon' && b.tag === 'TCon' && a.name === b.name) return;
  if (a.tag === 'TVar' && b.tag === 'TVar' && a.id === b.id) return;
  if (a.tag === 'TSkol' && b.tag === 'TSkol' && a.id === b.id) return;
  if (a.tag === 'TMeta' && b.tag === 'TMeta' && a.id === b.id) return;
  if (a.tag === 'TApp' && b.tag === 'TApp') {
    unify(a.left, b.left);
    unify(a.right, b.right);
    return;
  }
  if (a.tag === 'TForall' && b.tag === 'TForall' && a.tvs.length === b.tvs.length) {
    const sks = a.tvs.map(() => freshTSkol());
    const rho1 = substTVars(a.tvs, sks, a.type);
    const rho2 = substTVars(b.tvs, sks, b.type);
    unify(rho1, rho2);
    const sk1 = freeSkolems(prune(a));
    const sk2 = freeSkolems(prune(b));
    if (sks.some(i => sk1.indexOf(i) >= 0 || sk2.indexOf(i) >= 0))
      return terr(`type not polymorphic enough in unify: ${showType(a)} ~ ${showType(b)}`);
    return;
  }
  if (a.tag === 'TMeta') return unifyTMeta(a, b);
  if (b.tag === 'TMeta') return unifyTMeta(b, a);
  return terr(`cannot unify: ${showType(a)} ~ ${showType(b)}`);
};
const unifyTMeta = (tv, t) => {
  if (tv.type) return unify(tv.type, t);
  if (t.tag === 'TMeta' && t.type) return unify(tv, t.type);
  if (containsTMeta(tv, t))
    return `occurs check failed: ${showType(tv)} in ${showType(t)}`;
  tv.type = t;
};

module.exports = {
  matchfun,
  subsume,
  unify,
};
