const terr = msg => { throw new TypeError(msg) };

// names
let _id = 0;
const freshId = () => _id++;
const resetId = () => { _id = 0 };

// types
const TVar = id => ({ tag: 'TVar', id });
const TMeta = id => ({ tag: 'TMeta', id, type: null });
const TCon = name => ({ tag: 'TCon', name });
const TApp = (left, right) => ({ tag: 'TApp', left, right });
const TForall = (id, type) => ({ tag: 'TForall', id, type });

const TFunC = TCon('->');
const TFun = (left, right) => TApp(TApp(TFunC, left), right);
const tfunL = t => t.left.right;
const tfunR = t => t.right;
const isTFun = t => t.tag === 'TApp' && t.left.tag === 'TApp' &&
  t.left.left === TFunC;

const showType = t => {
  if (t.tag === 'TVar') return `'${t.id}`;
  if (t.tag === 'TMeta') return `?${t.id}`;
  if (t.tag === 'TCon') return t.name;
  if (isTFun(t))
    return `(${showType(tfunL(t))} -> ${showType(tfunR(t))})`;
  if (t.tag === 'TApp')
    return `(${showType(t.left)} ${showType(t.right)})`;
  if (t.tag === 'TForall')
    return `(forall ${t.id}. ${showType(t.type)})`;
};

const prune = t => {
  if (t.tag === 'TMeta')
    return t.type ? (t.type = prune(t.type)) : t;
  if (t.tag === 'TApp')
    return TApp(prune(t.left), prune(t.right));
  if (t.tag === 'TForall')
    return TForall(t.id, prune(t.type));
  return t;
};

const substTVar = (x, s, t) => {
  if (t.tag === 'TVar') return t.id === x ? s : t;
  if (t.tag === 'TMeta') return t.type ? substTVar(x, s, t.type) : t;
  if (t.tag === 'TApp')
    return TApp(substTVar(x, s, t.left), substTVar(x, s, t.right));
  if (t.tag === 'TForall')
    return t.id === x ? t : TForall(t.id, substTVar(x, s, t.type));
  return t;
};

const hasTMeta = (x, t) => {
  if (x === t) return true;
  if (t.tag === 'TMeta') return t.type && hasTMeta(x, t.type);
  if (t.tag === 'TApp')
    return hasTMeta(x, t.left) || hasTMeta(x, t.right);
  if (t.tag === 'TForall') return hasTMeta(x, t.type);
  return false;
};

const tmetas = (t, tms, res = []) => {
  if (t.tag === 'TMeta') {
    if (t.type) return tmetas(t.type, tms, res);
    if (tms.indexOf(t) >= 0 && res.indexOf(t) < 0) res.push(t);
    return res;
  }
  if (t.tag === 'TApp')
    return tmetas(t.right, tms, tmetas(t.left, tms, res));
  if (t.tag === 'TForall') return tmetas(t.type, tms, res);
  return res;
};

const tforall = (ids, t) => ids.reduceRight((x, y) => TForall(y, x), t);

const isMono = t => {
  if (t.tag === 'TMeta') return t.type ? isMono(t.type) : true;
  if (t.tag === 'TApp') return isMono(t.left) && isMono(t.right);
  if (t.tag === 'TForall') return false;
  return true;
};

// terms
const Var = name => ({ tag: 'Var', name });
const Abs = (name, body) => ({ tag: 'Abs', name, body });
const App = (left, right) => ({ tag: 'App', left, right });
const Ann = (term, type) => ({ tag: 'Ann', term, type });

const showTerm = t => {
  if (t.tag === 'Var') return t.name;
  if (t.tag === 'Abs') return `(\\${t.name} -> ${showTerm(t.body)})`;
  if (t.tag === 'App')
    return `(${showTerm(t.left)} ${showTerm(t.right)})`;
  if (t.tag === 'Ann')
    return `(${showTerm(t.term)} : ${showType(t.type)})`;
};

const flattenApp = t => {
  const r = [];
  while (t.tag === 'App') {
    r.push(t.right);
    t = t.left;
  }
  return [t, r.reverse()];
};

// context
const Marker = id => ({ tag: 'Marker', id });
const context = [];

const showElem = m =>
  m.tag === 'Marker' ? `|>${m.id}` : showType(m);
const showContext = () => `[${context.map(showElem).join(', ')}]`;

const mark = () => {
  const m = Marker(freshId());
  context.push(m);
  return m;
};
const drop = m => context.splice(context.indexOf(m), context.length);

const freshTVar = () => {
  const tv = TVar(freshId());
  context.push(tv);
  return tv;
};
const freshTMeta = () => {
  const tv = TMeta(freshId());
  context.push(tv);
  return tv;
};

const findTVar = tv => {
  const id = tv.id;
  for (let i = context.length - 1; i >= 0; i--) {
    const c = context[i];
    if (c.tag === 'TVar' && c.id === id) return i;
  }
  return -1;
};

const replace = (i, a) => context.splice(i, 1, a);
const replace2 = (i, a, b) => context.splice(i, 1, a, b);

// unification
const solve = (x, i, t) => {
  x.type = t;
  context.splice(i, 1);
};
const unifyTMeta = (x, t) => {
  console.log(`unifyTMeta ${showType(x)} := ${showType(t)} | ${showContext()}`);
  if (x === t) return;
  if (x.type) return unify(x.type, t);
  const i = context.indexOf(x);
  if (i < 0) return terr(`undefined tmeta ${showType(x)}`);
  if (t.tag === 'TCon') return solve(x, i, t);
  if (t.tag === 'TMeta') {
    if (t.type) return unifyTMeta(x, t.type);
    const j = context.indexOf(t);
    if (j < 0) return terr(`undefined tmeta ${showType(t)}`);
    return i < j ? solve(t, j, x) : solve(x, i, t);
  }
  if (t.tag === 'TVar') {
    const j = findTVar(t);
    if (j < 0) return terr(`undefined tvar ${showType(t)}`);
    if (j > i)
      return terr(`tvar out of scope ${showType(x)} := ${showType(t)}`);
    return solve(x, i, t);
  }
  if (hasTMeta(x, t))
    return terr(`occurs check failed ${showType(x)} := ${showType(t)}`);
  if (t.tag === 'TApp') {
    const a = TMeta(freshId());
    const b = TMeta(freshId());
    replace2(i, a, b);
    const ty = TApp(a, b);
    x.type = ty;
    return unify(ty, t);
  }
  if (t.tag === 'TForall') {
    const m = mark();
    const tv = TVar(freshId());
    const a = TMeta(freshId());
    replace2(i, tv, a);
    x.type = TForall(tv.id, a);
    unifyTMeta(a, substTVar(t.id, tv, t.type));
    drop(m);
    return;
  }
  return terr(`unification failed ${showType(x)} := ${showType(t)}`);
};
const unify = (a, b) => {
  console.log(`unify ${showType(a)} ~ ${showType(b)} | ${showContext()}`);
  if (a === b) return;
  if (a.tag === 'TMeta') return unifyTMeta(a, b);
  if (b.tag === 'TMeta') return unifyTMeta(b, a);
  if (a.tag === 'TApp' && b.tag === 'TApp') {
    unify(a.left, b.left);
    unify(a.right, b.right);
    return;
  }
  if (a.tag === 'TForall' && b.tag === 'TForall') {
    const m = mark();
    const tv = freshTVar();
    unify(substTVar(a.id, tv, a.type), substTVar(b.id, tv, b.type));
    drop(m);
    return;
  }
  if (a.tag === 'TCon' && b.tag === 'TCon' && a.name === b.name) return;
  if (a.tag === 'TVar' && b.tag === 'TVar' && a.id === b.id) return;
  return terr(`unification failed ${showType(a)} ~ ${showType(b)}`);
};

const inst = t => {
  if (t.tag === 'TMeta') return t.type ? inst(t.type) : t;
  if (t.tag === 'TForall') {
    const m = freshTMeta();
    return inst(substTVar(t.id, m, t.type));
  }
  return t;
};
const skol = t => {
  if (t.tag === 'TMeta') return t.type ? skol(t.type) : t;
  if (t.tag === 'TForall') {
    const tv = freshTVar();
    return skol(substTVar(t.id, tv, t.type));
  }
  return t;
};
const subsume = (a, b) => {
  console.log(`subsume ${showType(a)} <: ${showType(b)} | ${showContext()}`);
  const ta = inst(a);
  const m = mark();
  const tb = skol(b);
  unify(ta, tb);
  drop(m);
};

// wf
const wfType = t => {
  if (t.tag === 'TVar') {
    if (findTVar(t) < 0) return terr(`undefined tvar ${showType(t)}`);
    return;
  }
  if (t.tag === 'TMeta') {
    if (t.type) return wfType(t.type);
    if (context.indexOf(t) < 0)
      return terr(`undefined tmeta ${showType(t)}`);
    return;
  }
  if (t.tag === 'TFun') {
    wfType(t.left);
    wfType(t.right);
    return;
  }
  if (t.tag === 'TForall') {
    const m = mark();
    const tv = freshTVar();
    wfType(substTVar(t.id, tv, t.type));
    drop(m);
    return;
  }
};

// env
const Nil = { tag: 'Nil' };
const Cons = (head, tail) => ({ tag: 'Cons', head, tail });
const extend = (k, v, l) => Cons([k, v], l);
const lookup = (k, l) => {
  let c = l;
  while (c.tag === 'Cons') {
    const [k2, v] = c.head;
    if (k === k2) return v;
    c = c.tail;
  }
  return null;
};
const listFrom = a => a.reduceRight((x, y) => Cons(y, x), Nil);
function list() { return listFrom(Array.from(arguments)) }

// inference
const generalize = (m, t) => {
  const dropped = drop(m);
  const tms = tmetas(t, dropped.filter(t => t.tag === 'TMeta'));
  const l = tms.length;
  const tvs = Array(l);
  for (let i = 0; i < l; i++) {
    const c = tms[i];
    const id = freshId();
    c.type = TVar(id);
    tvs[i] = id;
  }
  return tforall(tvs, t);
};

const infer = (t, env = Nil) => {
  const m = mark();
  const ty = synth(env, t);
  return prune(generalize(m, ty));
};

const synth = (env, term) => {
  console.log(`synth ${showTerm(term)}`);
  if (term.tag === 'Var') {
    const t = lookup(term.name, env);
    if (!t) return terr(`undefined var ${term.name}`);
    return t;
  }
  if (term.tag === 'Ann') {
    wfType(term.type);
    check(env, term.term, term.type);
    return term.type;
  }
  if (term.tag === 'App') {
    const [f, as] = flattenApp(term);
    const ty = synth(env, f);
    const a = collectArgs(inst(ty), as);
    console.log(a);
    return synthapp(env, ty, term.right);
  }
  if (term.tag === 'Abs') {
    const a = freshTMeta();
    const m = mark();
    const ty = synth(extend(term.name, a, env), term.body);
    if (a.type && !isMono(a.type))
      return terr(`poly type infered for abstraction argument ${showTerm(term)}: ${showType(a.type)}`);
    return generalize(m, TFun(a, ty));
  }
  return terr(`cannot synth ${showTerm(term)}`);
};
const check = (env, term, type) => {
  console.log(`check ${showTerm(term)} : ${showType(type)}`);
  if (type.tag === 'TForall') {
    const m = mark();
    const tv = freshTVar();
    check(env, term, substTVar(type.id, tv, type.type));
    drop(m);
    return;
  }
  if (term.tag === 'Abs' && isTFun(type)) {
    const m = mark();
    check(extend(term.name, tfunL(type), env), term.body, tfunR(type));
    drop(m);
    return;
  }
  const ty = synth(env, term);
  subsume(ty, type);
};
const synthapp = (env, type, term) => {
  console.log(`synthapp ${showType(type)} @ ${showTerm(term)}`);
  if (type.tag === 'TForall') {
    const tm = freshTMeta();
    return synthapp(env, substTVar(type.id, tm, type.type), term);
  }
  if (isTFun(type)) {
    check(env, term, tfunL(type));
    return tfunR(type);
  }
  if (type.tag === 'TMeta') {
    if (type.type) return synthapp(env, type.type, term);
    const i = context.indexOf(type);
    if (i < 0) return terr(`undefined tmeta ${showType(type)}`);
    const a = TMeta();
    const b = TMeta();
    replace(i, [b, a]);
    type.type = TFun(a, b);
    check(env, term, a);
    return b;
  }
  return terr(`cannot synthapp ${showType(type)} @ ${showTerm(term)}`);
};

const collectArgs = (f, args, res = []) => {
  if (args.length === 0) return [res, f, args];
  if (f.tag === 'TMeta') {
    if (f.type) return collectArgs(f, args, res);
    const a = freshTMeta();
    const b = freshTMeta();
    f.type = TFun(a, b);
    const arg = args.shift();
    res.push([arg, a]);
    return collectArgs(b, args, res);
  }
  if (isTFun(f)) {
    const arg = args.shift();

  }
  
};

// testing
const v = Var;
const tv = TVar;
const abs = (ns, t) => ns.reduceRight((x, y) => Abs(y, x), t);
const appFrom = a => a.reduce(App);
function app() { return appFrom(Array.from(arguments)) }
const tappFrom = a => a.reduce(TApp);
function tapp() { return tappFrom(Array.from(arguments)) }
const tfunFrom = a => a.reduceRight((x, y) => TFun(y, x));
function tfun() { return tfunFrom(Array.from(arguments)) }

const tid = tforall([0], tfun(tv(0), tv(0)));
const List = TCon('List');

const env = list(
  ['singleton', tforall([0], tfun(tv(0), tapp(List, tv(0))))],
);
const term = app(v('singleton'), abs(['x'], v('x')));
console.log(showTerm(term));
const ty = infer(term, env);
console.log(showType(ty));
console.log(showTerm(term));
