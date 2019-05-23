const { resetId, annotAny, prune, isMono, isSigma, TFun, matchTFun } = require('./types');
const { AAbs, App, Var, isAnnot, flattenApp } = require('./terms');
const { terr } = require('./util');
const { matchfunN, subsume } = require('./unify');
const { generalize, instantiateAnnot, instantiate, pickArg } = require('./operations');

const Inst = { tag: 'Inst' };
const Gen = { tag: 'Gen' };
const maybeInst = (ex, ty) =>
  ex.tag === 'Inst' ? instantiate(ty) : ty;
const maybeGen = (ex, env, ty) =>
  ex.tag === 'Gen' ? generalize(env, ty) : ty;
const maybeInstOrGen = (ex, env, ty) =>
  ex.tag === 'Gen' ? generalize(env, ty) : instantiate(ty);

const infer = (env, term) => {
  resetId();
  const ty = synth(null, Gen, env, term);
  return prune(ty);
};

const extend = (n, t, e) => {
  const o = Object.create(e);
  o[n] = t;
  return o;
};

const synth = (prop, ex, env, term) => {
  if (term.tag === 'Var') {
    if (!env[term.name]) return terr(`undefined var ${term.name}`);
    return maybeInst(ex, env[term.name]);
  }
  if (term.tag === 'Let') {
    const ty = synth(null, Gen, env, term.val);
    return synth(prop, ex, extend(term.name, ty, env), term.body);
  }
  if (term.tag === 'Abs') {
    const [proparg, _, __] = propFun(prop);
    return synth(
      prop,
      ex,
      env,
      AAbs(term.name, proparg ? Annot([], proparg) : annotAny, term.body),
    );
  }
  if (term.tag === 'Ann') {
    const [_, atp] = instantiateAnnot(term.annot);
    const ty = synth(
      atp,
      isSigma(atp) ? Gen : Inst,
      env,
      term.term,
    );
    subsume(atp, ty);
    return prune(atp);
  }
  if (term.tag === 'Rigid')
    return synth(Gen, env, term.term);
  if (term.tag === 'App') {
    const [f, args] = flattenApp(term);
    const fty = synth(null, Inst, env, f);
    return inferApp(prop, ex, env, fty, args);
  }
  if (term.tag === 'AAbs') {
    const [_, propres, exres] = propFun(prop);
    const [some, ty1] = instantiateAnnot(term.annot);
    const ty2 = synth(propres, exres, extend(term.name, ty1, env), term.body);
    if (some.some(t => !isMono(prune(t))))
      return terr(`unannotated parameters used polymorphically in ${showTerm(term)}`);
    return maybeGen(ex, env, TFun(ty1, ty2));    
  }
};

const inferApp = (prop, ex, env, fty, args) => {
  const [tpars, res] = matchfunN(fty, args.length);
  propApp(prop, res, tpars.length === args.length);
  subsumeInferN(env, zip(tpars, args));
  const argsLeft = args.slice(tpars.length);
  if (argsLeft.length === 0)
    return maybeInstOrGen(ex, env, res);
  return inferApp(prop, ex, env, res, argsLeft);
};

const subsumeInferN = (env, tps) => {
  if (tps.length === 0) return;
  const [tpar, arg, rest] = pickArg(tps);
  const targ = synth(tpar, isSigma(tpar) ? Gen : Inst, env, arg);
  (isAnnot(arg) ? unify(tpar, targ) : subsume(tpar, targ));
  subsumeInferN(env, rest);
};

const zip = (a, b) => {
  const l = Math.min(a.length, b.length);
  const r = Array(l);
  for (let i = 0; i < l; i++)
    r[i] = [a[i], b[i]];
  return r;
};

const propApp = (prop, ty, fapp) => {
  const isuni = ty.tag === 'TMeta' && !ty.type;
  if (prop && fapp && !isuni) {
    const rho = instantiate(prop);
    subsume(rho, ty);
  }
};
const propFun = prop => {
  if (!prop) return [null, null, Inst];
  const rho = prune(instantiate(prop));
  const m = matchTFun(rho);
  if (m) return [m.left, m.right, isSigma(m.right) ? Gen : Inst];
  return [null, null, Inst];
};

module.exports = { infer };
