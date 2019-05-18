const { freshTMeta, substTVars, freshTSkol, freeTMeta, freeTMetaInEnv, prune, freshId, TForall, TVar } = require('./types');

const instantiate = ty => {
  const t = prune(ty);
  if (t.tag !== 'TForall') return t;
  const tvs = t.tvs.map(() => freshTMeta());
  return substTVars(t.tvs, tvs, t.type);
};

const instantiateAnnot = a => {
  if (a.tvs.length === 0) return [[], a.type];
  const tvs = a.tvs.map(() => freshTMeta());
  return [tvs, substTVars(a.tvs, tvs, a.type)];
};

const skolemize = ty => {
  const t = prune(ty);
  if (t.tag !== 'TForall') return [[], t];
  const tvs = t.tvs.map(() => freshTSkol());
  return [tvs, substTVars(t.tvs, tvs, t.type)];
};

const generalize = (env, tyy) => {
  const ty = prune(tyy);
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
  instantiate,
  instantiateAnnot,
  skolemize,
  generalize,
};
