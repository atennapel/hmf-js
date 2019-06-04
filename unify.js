const { terr } = require('./util');
const {
  showType,
  freshId,
  freshTSkol,
  freshTMeta,
  substTVars,
  occursTMeta,
  occursTSkol,
  instantiate,
  skolemize,
  isTFun,
  TFun,
  prune,
  freeTMeta,
  freeTMetaInEnv,
  TVar,
  TForall,
} = require('./types');

const unifyTMeta = (m, t) => {
  if (m.type) return unify(prune(m), t);
  if (t.tag === 'TMeta' && t.type) return unify(m, prune(t));
  if (occursTMeta(m, t))
    return terr(`${showType(m)} occurs in ${showType(t)}`);
  m.type = t;
};
const unify = (t1, t2) => {
  console.log(`unify ${showType(t1)} ~ ${showType(t2)}`);
  if (t1 === t2) return;
  if (t1.tag === 'TMeta') return unifyTMeta(t1, t2);
  if (t2.tag === 'TMeta') return unifyTMeta(t2, t1);
  if (t1.tag === 'TApp' && t2.tag === 'TApp') {
    unify(t1.left, t2.left);
    unify(t1.right, t2.right);
    return;
  }
  if (t1.tag === 'TForall' && t2.tag === 'TForall' &&
    t1.ids.length === t2.ids.length) {
    const sks = t1.ids.map(() => freshTSkol());
    const rho1 = substTVars(t1.ids, sks, t1.type);
    const rho2 = substTVars(t2.ids, sks, t2.type);
    unify(rho1, rho2);
    if (occursTSkol(sks, prune(t1)) || occursTSkol(sks, prune(t2)))
      return terr(`skolem check fail [${sks.join(' ')}] in ${showType(t1)} ~ ${showType(t2)}`);
    return;
  }
  return terr(`cannot unify ${showType(t1)} ~ ${showType(t2)}`);
};

const subsume = (t1, t2) => {
  console.log(`subsume ${showType(t1)} <: ${showType(t2)}`);
  const rho1 = instantiate(t1);
  const [sks, rho2] = skolemize(t2);
  unify(rho1, rho2);
  if (occursTSkol(sks, prune(t1)) || occursTSkol(sks, prune(t2)))
    return terr(`skolem check fail [${sks.map(showType).join(' ')}] in ${showType(prune(t1))} <: ${showType(prune(t2))}`);
};

const matchfun = ty => {
  const rho = instantiate(prune(ty));
  if (isTFun(rho)) return rho;
  if (rho.tag === 'TMeta') {
    const left = freshTMeta();
    const right = freshTMeta();
    return rho.type = TFun(left, right);
  }
  return terr(`applying non-function: ${showType(ty)}`);
};

const generalize = (env, tyy) => {
  const ty = prune(tyy);
  console.log(`generalize ${showType(tyy)}`);
  const tms = freeTMeta(ty);
  const envtms = freeTMetaInEnv(env);
  const gtms = [];
  for (let i = 0, l = tms.length; i < l; i++) {
    if (envtms.indexOf(tms[i]) < 0) gtms.push(tms[i]);
  }
  if (gtms.length === 0) return prune(ty);
  const ftvs = gtms.map(tm => {
    const tv = freshId();
    tm.type = TVar(tv);
    return tv;
  });
  return TForall(ftvs, prune(ty));
};

module.exports = {
  unify,
  subsume,
  matchfun,
  skolemize,
  instantiate,
  generalize,
};
