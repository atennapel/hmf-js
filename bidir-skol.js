const terr = msg => { throw new TypeError(msg) };
let log = true;

// names
let _id = 0;
const freshId = () => _id++;
const resetId = () => { _id = 0 };

// types
const TVar = id => ({ tag: 'TVar', id });
const TMeta = id => ({ tag: 'TMeta', id, type: null });
const TSkol = id => ({ tag: 'TSkol', id });
const TCon = name => ({ tag: 'TCon', name });
const TApp = (left, right) => ({ tag: 'TApp', left, right });
const TForall = (id, type) => ({ tag: 'TForall', id, type });

const freshTMeta = () => TMeta(freshId());
const freshTSkol = () => TSkol(freshId());

const TFunC = TCon('->');
const TFun = (left, right) => TApp(TApp(TFunC, left), right);
const tfunL = t => t.left.right;
const tfunR = t => t.right;
const isTFun = t => t.tag === 'TApp' && t.left.tag === 'TApp' &&
  t.left.left === TFunC;

const showType = t => {
  if (t.tag === 'TVar') return `'${t.id}`;
  if (t.tag === 'TMeta') return `?${t.id}`;
  if (t.tag === 'TSkol') return `\$${t.id}`;
  if (t.tag === 'TCon') return t.name;
  if (isTFun(t))
    return `(${showType(tfunL(t))} -> ${showType(tfunR(t))})`;
  if (t.tag === 'TApp')
    return `(${showType(t.left)} ${showType(t.right)})`;
  if (t.tag === 'TForall')
    return `(forall ${t.id}. ${showType(t.type)})`;
};
const showTypeP = t => showType(prune(t));

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

const hasTSkol = (x, t) => {
  if (x === t) return true;
  if (t.tag === 'TMeta') return t.type && hasTSkol(x, t.type);
  if (t.tag === 'TApp')
    return hasTSkol(x, t.left) || hasTSkol(x, t.right);
  if (t.tag === 'TForall') return hasTSkol(x, t.type);
  return false;
};
const hasAnyTSkol = (xs, t) => {
  if (t.tag === 'TSkol' && xs.indexOf(t) >= 0) return true;
  if (t.tag === 'TMeta') return t.type && hasAnyTSkol(xs, t.type);
  if (t.tag === 'TApp')
    return hasAnyTSkol(xs, t.left) || hasAnyTSkol(xs, t.right);
  if (t.tag === 'TForall') return hasAnyTSkol(xs, t.type);
  return false;
};

const tmetas = (t, tms, res = []) => {
  if (t.tag === 'TMeta') {
    if (t.type) return tmetas(t.type, tms, res);
    if (tms.indexOf(t) < 0 && res.indexOf(t) < 0) res.push(t);
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

const isTMeta = t => t.tag === 'TMeta' && (!t.type || isTMeta(t.type));

// terms
const Var = name => ({ tag: 'Var', name });
const Abs = (name, body, type = null) => ({ tag: 'Abs', name, type, body });
const App = (left, right) => ({ tag: 'App', left, right });
const Ann = (term, type) => ({ tag: 'Ann', term, type });

const showTerm = t => {
  if (t.tag === 'Var') return t.name;
  if (t.tag === 'Abs')
    return t.type ? `(\\(${t.name} : ${showType(t.type)}) -> ${showTerm(t.body)})` : `(\\${t.name} -> ${showTerm(t.body)})`;
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

// unification
const unifyTMeta = (x, t) => {
  if (log) console.log(`unifyTMeta ${showType(x)} := ${showType(t)}`);
  if (x.type) return unify(x.type, t);
  if (t.tag === 'TMeta' && t.type) return unifyTMeta(x, t.type);
  if (hasTMeta(x, t)) return terr(`occurs check failed: ${showTypeP(x)} in ${showTypeP(t)}`);
  x.type = t;
};
const unify = (a, b) => {
  if (log) console.log(`unify ${showType(a)} ~ ${showType(b)}`);
  if (a === b) return;
  if (a.tag === 'TMeta') return unifyTMeta(a, b);
  if (b.tag === 'TMeta') return unifyTMeta(b, a);
  if (a.tag === 'TApp' && b.tag === 'TApp') {
    unify(a.left, b.left);
    unify(a.right, b.right);
    return;
  }
  if (a.tag === 'TForall' && b.tag === 'TForall') {
    const sk = freshTSkol();
    unify(substTVar(a.id, sk, a.type), substTVar(b.id, sk, b.type));
    if (hasTSkol(sk, a)) return terr(`${showTypeP(a)} not polymorphic enough in ${showTypeP(a)} ~ ${showTypeP(b)}`);
    if (hasTSkol(sk, b)) return terr(`${showTypeP(b)} not polymorphic enough in ${showTypeP(a)} ~ ${showTypeP(b)}`);
    return;
  }
  if (a.tag === 'TCon' && b.tag === 'TCon' && a.name === b.name) return;
  if (a.tag === 'TSkol' && b.tag === 'TSkol' && a.id === b.id) return;
  return terr(`unification failed ${showTypeP(a)} ~ ${showTypeP(b)}`);
};

const inst = t => {
  if (t.tag === 'TMeta') return t.type ? inst(t.type) : t;
  if (t.tag === 'TForall') {
    const m = freshTMeta();
    return inst(substTVar(t.id, m, t.type));
  }
  return t;
};
const skol = (t, sk = []) => {
  if (t.tag === 'TMeta') return t.type ? skol(t.type, sk) : t;
  if (t.tag === 'TForall') {
    const tv = freshTSkol();
    sk.push(tv);
    return skol(substTVar(t.id, tv, t.type), sk);
  }
  return t;
};
const subsume = (a, b) => {
  if (log) console.log(`subsume ${showTypeP(a)} <: ${showTypeP(b)}`);
  const sks = [];
  const tb = skol(b, sks);
  const ta = inst(a);
  unify(ta, tb);
  if (hasAnyTSkol(sks, a)) return terr(`${showTypeP(a)} not polymorphic enough in ${showTypeP(a)} <: ${showTypeP(b)}`);
  if (hasAnyTSkol(sks, b)) return terr(`${showTypeP(b)} not polymorphic enough in ${showTypeP(a)} <: ${showTypeP(b)}`);
};

const matchfun = ty => {
  if (isTFun(ty)) return [tfunL(ty), tfunR(ty)];
  const a = freshTMeta();
  const b = freshTMeta();
  unify(TFun(a, b), ty);
  return [a, b];
};

// wf
const wfType = t => {
  if (t.tag === 'TMeta') return terr(`type annotation cannot contain TMeta: ${showTypeP(t)}`);
  if (t.tag === 'TSkol') return terr(`type annotation cannot contain TSkol: ${showTypeP(t)}`);
  if (t.tag === 'TApp') {
    wfType(t.left);
    wfType(t.right);
    return;
  }
  if (t.tag === 'TForall') {
    wfType(t.type);
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

const each = (l, f) => {
  let c = l;
  while (c.tag === 'Cons') {
    f(c.head[0], c.head[1]);
    c = c.tail;
  }
};

const tmetasEnv = (env, tms = []) => {
  each(env, (_, v) => tmetas(v, [], tms));
  return tms;
};

// inference
const generalize = (env, t) => {
  const tmsenv = tmetasEnv(env);
  const tms = tmetas(t, tmsenv);
  const l = tms.length;
  const tvs = Array(l);
  for (let i = 0; i < l; i++) {
    const c = tms[i];
    const id = freshId();
    c.type = TVar(id);
    tvs[i] = id;
  }
  return tforall(tvs, prune(t));
};

const escapeCheckEnv = (sks, env) => {
  each(env, (k, v) => {
    if (hasAnyTSkol(sks, v))
      terr(`TSkol escaped in ${showTypeP(v)} in ${k}`);
  });
};

const infer = (t, env = Nil) => {
  resetId();
  return synth(env, t);
};

const synth = (env, term) => {
  if (log) console.log(`synth ${showTerm(term)}`);
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
    const res = synthapp(env, ty, as);
    return generalize(env, res);
  }
  if (term.tag === 'Abs') {
    const a = term.type || freshTMeta();
    const ty = synth(extend(term.name, a, env), term.body);
    if (a.tag === 'TMeta' && a.type && !isMono(a.type))
      return terr(`poly type infered for abstraction argument ${showTerm(term)}: ${showTypeP(a.type)}`);
    return generalize(env, TFun(a, inst(ty)));
  }
  return terr(`cannot synth ${showTerm(term)}`);
};
const check = (env, term, type) => {
  if (log) console.log(`check ${showTerm(term)} : ${showTypeP(type)}`);
  if (term.tag === 'Abs' && !term.type) {
    const sks = [];
    const itype = skol(type, sks);
    const [a, b] = matchfun(itype);
    check(extend(term.name, a, env), term.body, b);
    if (hasAnyTSkol(sks, type)) return terr(`TSkol escape in ${showTerm(term)} : ${showTypeP(type)}`);
    escapeCheckEnv(sks, env);
    return;
  }
  if (term.tag === 'App') {
    const [f, as] = flattenApp(term);
    const ty = synth(env, f);
    const res = synthapp(env, ty, as, type);
    subsume(res, type);
    return;
  }
  const ty = synth(env, term);
  subsume(ty, type);
};
const synthapp = (env, type, args, extype) => {
  if (log) console.log(`synthapp ${showType(type)} @ [${args.map(showTerm).join(', ')}]${extype ? ` : ${showTypeP(extype)}` : ''}`);
  if (args.length === 0) return type;
  const [pars, ret, resargs] = collectArgs(inst(type), args);
  if (extype && resargs.length === 0)
    (isTMeta(ret) ? unify : subsume)(ret, extype);
  while (pars.length > 0) {
    let found = false;
    for (let i = 0, l = pars.length; i < l; i++) {
      const [arg, party] = pars[i];
      if (!isTMeta(party)) {
        found = true;
        pars.splice(i, 1);
        check(env, arg, party);
        break;
      }
    }
    if (!found) {
      const [arg, party] = pars.shift();
      check(env, arg, party);
    }
  }
  return synthapp(env, ret, resargs);
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
    res.push([arg, tfunL(f)]);
    return collectArgs(tfunR(f), args, res);
  }
  return [res, f, args];
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
const Bool = TCon('Bool');
const Nat = TCon('Nat');
const Pair = TCon('Pair');
const ST = TCon('ST');

const env = list(
  ['true', Bool],
  ['zero', Nat],
  ['single', tforall([0], tfun(tv(0), tapp(List, tv(0))))],
  ['const', tforall([0, 1], tfun(tv(0), tv(1), tv(0)))],
  ['id', tid],
  ['ids', tapp(List, tid)],
  ['Nil', tforall([0], tapp(List, tv(0)))],
  ['Cons', tforall([0], tfun(tv(0), tapp(List, tv(0)), tapp(List, tv(0))))],
  ['snoc', tforall([0], tfun(tapp(List, tv(0)), tv(0), tapp(List, tv(0))))],
  ['choose', tforall([0], tfun(tv(0), tv(0), tv(0)))],
  ['auto', tfun(tid, tid)],
  ['auto2', tforall([0], tfun(tid, tv(0), tv(0)))],
  ['f', tforall([0], tfun(tfun(tv(0), tv(0)), tapp(List, tv(0)), tv(0)))],
  ['poly', tfun(tid, tapp(Pair, Nat, Bool))],
  ['Pair', tforall([0, 1], tfun(tv(0), tv(1), tapp(Pair, tv(0), tv(1))))],
  ['head', tforall([0], tfun(tapp(List, tv(0)), tv(0)))],
  ['length', tforall([0], tfun(tapp(List, tv(0)), Nat))],
  ['tail', tforall([0], tfun(tapp(List, tv(0)), tapp(List, tv(0))))],
  ['append', tforall([0], tfun(tapp(List, tv(0)), tapp(List, tv(0)), tapp(List, tv(0))))],
  ['inc', tfun(Nat, Nat)],
  ['g', tforall([0], tfun(tapp(List, tv(0)), tapp(List, tv(0)), tv(0)))],
  ['map', tforall([0, 1], tfun(tfun(tv(0), tv(1)), tapp(List, tv(0)), tapp(List, tv(1))))],
  ['app', tforall([0, 1], tfun(tfun(tv(0), tv(1)), tv(0), tv(1)))],
  ['revapp', tforall([0, 1], tfun(tv(0), tfun(tv(0), tv(1)), tv(1)))],
  ['runST', tforall([0], tfun(tforall([1], tapp(ST, tv(1), tv(0))), tv(0)))],
  ['argST', tforall([0], tapp(ST, tv(0), Nat))],
  ['h', tfun(Nat, tforall([0], tfun(tv(0), tv(0))))],
  ['k', tforall([0], tfun(tv(0), tapp(List, tv(0)), tv(0)))],
  ['lst', tapp(List, tforall([0], tfun(Nat, tv(0), tv(0))))],
  ['r', tfun(tforall([0], tfun(tv(0), tforall([1], tfun(tv(1), tv(1))))), Nat)],
);

const terms = [
  // A
  abs(['x'], v('x')),
  abs(['x', 'y'], v('x')),
  app(v('choose'), v('id')),
  app(v('choose'), v('Nil'), v('ids')),
  Abs('x', app(v('x'), v('x')), tid),
  app(v('id'), v('auto')),
  app(v('id'), v('auto2')),
  app(v('choose'), v('id'), v('auto')),
  app(v('choose'), v('id'), v('auto2')),
  app(v('choose'), Ann(v('id'), tfun(tid, tid)), v('auto2')), // ?
  app(v('f'), app(v('choose'), v('id')), v('ids')),
  app(v('f'), Ann(app(v('choose'), v('id')), tfun(tid, tid)), v('ids')),
  app(v('poly'), v('id')),
  app(v('poly'), abs(['x'], v('x'))),
  app(v('id'), v('poly'), abs(['x'], v('x'))),
  // B
  abs(['f'], app(v('Pair'), app(v('f'), v('zero')), app(v('f'), v('true')))),
  Abs('f', app(v('Pair'), app(v('f'), v('zero')), app(v('f'), v('true'))), tid),
  abs(['xs'], app(v('poly'), app(v('head'), v('xs')))),
  Abs('xs', app(v('poly'), app(v('head'), v('xs'))), tapp(List, tid)),
  // C
  app(v('length'), v('ids')),
  app(v('tail'), v('ids')),
  app(v('head'), v('ids')),
  app(v('single'), v('ids')),
  app(v('Cons'), v('id'), v('ids')), // X
  app(v('snoc'), v('ids'), v('id')),
  app(v('Cons'), abs(['x'], v('x')), v('ids')), // X
  app(v('snoc'), v('ids'), abs(['x'], v('x'))),
  app(v('append'), app(v('single'), v('inc')), app(v('single'), v('id'))),
  app(v('g'), app(v('single'), v('id')), v('ids')),
  app(v('map'), v('poly'), app(v('single'), v('id'))),
  app(v('map'), v('head'), app(v('single'), v('ids'))),
  // D
  app(v('app'), v('poly'), v('id')),
  app(v('revapp'), v('id'), v('poly')), // X
  app(v('runST'), v('argST')),
  app(v('app'), v('runST'), v('argST')),
  app(v('revapp'), v('argST'), v('runST')), // X
  // E
  app(v('k'), v('h'), v('lst')),
  app(v('k'), abs(['x'], app(v('h'), v('x'))), v('lst')), // X
  app(v('r'), abs(['x', 'y'], v('y'))),
];

log = false;
terms.forEach(t => {
  try {
    console.log(`${showTerm(t)} => ${showType(infer(t, env))}`);
  } catch (err) {
    console.log(`${showTerm(t)} => ${err}`);
  }
});

/*
const term = app(v('Cons'), v('id'), v('ids'));
//const term = abs(['x'], app(v('h'), v('x')));
console.log(showType(infer(term, env)));
console.log(showTerm(term));
*/
