let id = 0;
const freshId = () => id++;
const resetId = () => { id = 0 };

const TCon = name => ({ tag: 'TCon', name });
const TVar = id => ({ tag: 'TVar', id });
const TMeta = id => ({ tag: 'TMeta', id, type: null });
const TSkol = id => ({ tag: 'TSkol', id });
const TApp = (left, right) => ({ tag: 'TApp', left, right });
const TForall = (ids, type) => ({ tag: 'TForall', ids, type });

const tappFrom = ts => ts.reduce(TApp);
function tapp() { return tappFrom(Array.from(arguments)) }

const tFun = TCon('->');
const TFun = (left, right) => TApp(TApp(tFun, left), right);
const isTFun = t =>
  t.tag === 'TApp' && t.left.tag === 'TApp' && t.left.left === tFun;
const tfunLeft = t => t.left.right;
const tfunRight = t => t.right;

const tfunFrom = ts => ts.reduceRight((x, y) => TFun(y, x));
function tfun() { return tfunFrom(Array.from(arguments)) }

const freshTMeta = () => TMeta(freshId());
const freshTSkol = () => TSkol(freshId());

const showType = t => {
  if (t.tag === 'TCon') return `${t.name}`;
  if (t.tag === 'TVar') return `'${t.id}`;
  if (t.tag === 'TMeta') return `?${t.id}`;
  if (t.tag === 'TSkol') return `\$${t.id}`;
  if (isTFun(t))
    return `(${showType(tfunLeft(t))} -> ${showType(tfunRight(t))})`;
  if (t.tag === 'TApp')
    return `(${showType(t.left)} ${showType(t.right)})`;
  if (t.tag === 'TForall')
    return `(forall ${t.ids.join(' ')}. ${showType(t.type)})`;
};

const isMono = ty => {
  if (ty.tag === 'TForall') return false;
  if (ty.tag === 'TApp') return isMono(ty.left) && isMono(ty.right);
  return true;
};
const isSigma = ty => ty.tag === 'TForall';

const prune = t => {
  if (t.tag === 'TMeta')
    return !t.type ? t : (t.type = prune(t.type));
  if (t.tag === 'TApp')
    return TApp(prune(t.left), prune(t.right));
  if (t.tag === 'TForall')
    return TForall(t.ids, prune(t.type));
  return t;
};

const substTVar = (map, ty) => {
  if (ty.tag === 'TVar')
    return map[ty.id] || ty;
  if (ty.tag === 'TApp')
    return TApp(substTVar(map, ty.left), substTVar(map, ty.right));
  if (ty.tag === 'TForall') {
    const n = {};
    for (let k in map) {
      if (ty.ids.indexOf(+k) < 0) n[k] = map[k];
    }
    return TForall(ty.ids, substTVar(n, ty.type));
  }
  return ty;
};
const substTVars = (old, nw, ty) => {
  const n = {};
  for (let i = 0, l = old.length; i < l; i++)
    n[old[i]] = nw[i];
  return substTVar(n, ty);
};

const occursTMeta = (m, t) => {
  if (m === t) return true;
  if (t.tag === 'TMeta' && t.type) return occursTMeta(m, prune(t));
  if (t.tag === 'TApp')
    return occursTMeta(m, t.left) || occursTMeta(m, t.right);
  if (t.tag === 'TForall') return occursTMeta(m, t.type);
  return false;
};
const occursTSkol = (m, t) => {
  if (t.tag === 'TSkol') return m.indexOf(t) >= 0;
  if (t.tag === 'TMeta' && t.type) return occursTSkol(m, prune(t));
  if (t.tag === 'TApp')
    return occursTSkol(m, t.left) || occursTSkol(m, t.right);
  if (t.tag === 'TForall') return occursTSkol(m, t.type);
  return false;
};

const instantiate = ty => {
  console.log(`instantiate ${showType(ty)}`);
  const t = prune(ty);
  if (t.tag !== 'TForall') return t;
  const tvs = t.ids.map(() => freshTMeta());
  return substTVars(t.ids, tvs, t.type);
};

const skolemize = ty => {
  console.log(`skolemize ${showType(ty)}`);
  const t = prune(ty);
  if (t.tag !== 'TForall') return [[], t];
  const tvs = t.ids.map(() => freshTSkol());
  return [tvs, substTVars(t.ids, tvs, t.type)];
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

module.exports = {
  freshId,
  resetId,

  TCon,
  TVar,
  TMeta,
  TSkol,
  TApp,
  TForall,

  tappFrom,
  tapp,

  tFun,
  TFun,
  tfunFrom,
  tfun,
  isTFun,
  tfunLeft,
  tfunRight,

  freshTMeta,
  freshTSkol,

  showType,
  isMono,
  isSigma,
  prune,
  substTVar,
  substTVars,
  
  occursTMeta,
  occursTSkol,

  instantiate,
  skolemize,

  freeTMeta,
  freeTMetaInEnv,
};
