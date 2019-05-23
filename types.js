const TForall = (tvs, type) => ({ tag: 'TForall', tvs, type });
const TCon = name => ({ tag: 'TCon', name });
const TApp = (left, right) => ({ tag: 'TApp', left, right });
const tappFrom = ts => ts.reduce(TApp);
function tapp() { return tappFrom(Array.from(arguments)) }
const TMeta = id => ({ tag: 'TMeta', id, type: null });
const TVar = id => ({ tag: 'TVar', id });
const TSkol = id => ({ tag: 'TSkol', id });

const tFun = TCon('->');
const TFun = (left, right) => TApp(TApp(tFun, left), right);
const matchTFun = ty => {
  if (ty.tag === 'TApp' && ty.left.tag === 'TApp' && ty.left.left === tFun)
    return { left: ty.left.right, right: ty.right };
  return null;
};
const tfunFrom = ts => ts.reduceRight((x, y) => TFun(y, x));
function tfun() { return tfunFrom(Array.from(arguments)) }

const Annot = (tvs, type) => ({ tvs, type });
const annotAny = Annot([0], TVar(0));

let id = 0;
const resetId = () => { id = 0 };
const freshId = () => id++;

const freshTMeta = () => TMeta(freshId());
const freshTVar = () => TVar(freshId());
const freshTSkol = () => TSkol(freshId());

const showAnnot = a =>
  a.tvs.length === 0 ? showType(a.type) :
  `exists ${a.tvs.join(' ')}. ${showType(a.type)}`;

const flattenTFun = t => {
  let c = t;
  let m = matchTFun(c);
  const r = [];
  while (m) {
    r.push(m.left);
    c = m.right;
    m = matchTFun(m.right);
  }
  r.push(c);
  return r;
};

const showType = t => {
  if (t.tag === 'TCon') return t.name;
  if (t.tag === 'TMeta') return `?${t.id}`;
  if (t.tag === 'TVar') return `'${t.id}`;
  if (t.tag === 'TSkol') return `\$${t.id}`;
  const m = matchTFun(t);
  if (m)
    return `(${flattenTFun(t).map(showType).join(' -> ')})`;
  if (t.tag === 'TApp')
    return `(${showType(t.left)} ${showType(t.right)})`;
  if (t.tag === 'TForall')
    return `(forall ${t.tvs.join(' ')}. ${showType(t.type)})`;
};

const prune = ty => {
  if (ty.tag === 'TMeta') {
    if (!ty.type) return ty;
    const t = prune(ty.type);
    ty.type = t;
    return t;
  }
  if (ty.tag === 'TApp') return TApp(prune(ty.left), prune(ty.right));
  if (ty.tag === 'TForall') return TForall(ty.tvs, prune(ty.type));
  return ty;
};

const isMono = ty => {
  if (ty.tag === 'TForall') return false;
  if (ty.tag === 'TApp') return isMono(ty.left) && isMono(ty.right);
  return true;
};
const isSigma = ty => ty.tag === 'TForall';

const substTVar = (map, ty) => {
  if (ty.tag === 'TVar')
    return map[ty.id] || ty;
  if (ty.tag === 'TApp')
    return TApp(substTVar(map, ty.left), substTVar(map, ty.right));
  if (ty.tag === 'TForall') {
    const n = {};
    for (let k in map) {
      if (ty.tvs.indexOf(+k) < 0) n[k] = map[k];
    }
    return TForall(ty.tvs, substTVar(n, ty.type));
  }
  return ty;
};
const substTVars = (old, nw, ty) => {
  const n = {};
  for (let i = 0, l = old.length; i < l; i++)
    n[old[i]] = nw[i];
  return substTVar(n, ty);
};

const freeSkolems = (ty, arr = []) => {
  if (ty.tag === 'TSkol') {
    if (arr.indexOf(ty.id) < 0) arr.push(ty.id);
    return arr;
  }
  if (ty.tag === 'TApp')
    return freeSkolems(ty.right, freeSkolems(ty.left, arr));
  if (ty.tag === 'TForall')
    return freeSkolems(ty.type, arr);
  return arr;
};

const freeTMeta = (ty, arr = []) => {
  if (ty.tag === 'TMeta') {
    if (arr.indexOf(ty) < 0) arr.push(ty);
    return arr;
  }
  if (ty.tag === 'TApp')
    return freeTMeta(ty.right, freeTMeta(ty.left, arr));
  if (ty.tag === 'TForall')
    return freeTMeta(ty.type, arr);
  return arr;
};
const freeTMetaInEnv = (env, arr = []) => {
  for (let k in env) freeTMeta(prune(env[k]), arr);
  return arr;
};

const containsTMeta = (tm, ty) => {
  if (tm === ty) return true;
  if (ty.tag === 'TMeta') {
    if (ty.type) return containsTMeta(tm, ty.type);
    return false;
  }
  if (ty.tag === 'TApp')
    return containsTMeta(tm, ty.left) || containsTMeta(tm, ty.right);
  if (ty.tag === 'TForall')
    return containsTMeta(tm, ty.type);
  return false;
};

module.exports = {
  TForall,
  TCon,
  TApp,
  tappFrom,
  tapp,
  TMeta,
  TVar,
  TSkol,

  tFun,
  TFun,
  matchTFun,
  tfunFrom,
  tfun,

  Annot,
  annotAny,

  resetId,
  freshId,
  freshTMeta,
  freshTSkol,
  freshTVar,

  showAnnot,
  showType,

  prune,
  isMono,
  isSigma,

  substTVar,
  substTVars,
  freeSkolems,
  freeTMeta,
  freeTMetaInEnv,
  containsTMeta,
};