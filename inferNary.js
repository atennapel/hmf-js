const { resetId, annotAny, prune, isMono, isSigma, TFun } = require('./types');
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
  return prune(synth(Gen, env, term));
};

const extend = (n, t, e) => {
  const o = Object.create(e);
  o[n] = t;
  return o;
};

const synth = (ex, env, term) => {
  if (term.tag === 'Var') {
    if (!env[term.name]) return terr(`undefined var ${term.name}`);
    return maybeInst(ex, env[term.name]);
  }
  if (term.tag === 'Let') {
    const ty = synth(Gen, env, term.val);
    return synth(ex, extend(term.name, ty, env), term.body);
  }
  if (term.tag === 'Abs')
    return synth(ex, env, AAbs(term.name, annotAny, term.body));
  if (term.tag === 'Ann') {
    const ty = synth(Gen, env, App(AAbs('x', term.annot, Var('x')), term.term));
    const [_, atp] = instantiateAnnot(term.annot);
    subsume(atp, ty);
    return prune(atp);
  }
  if (term.tag === 'Rigid')
    return synth(Gen, env, term.term);
  if (term.tag === 'App') {
    const [f, args] = flattenApp(term);
    const fty = synth(Inst, env, f);
    return inferApp(ex, env, fty, args);
  }
  if (term.tag === 'AAbs') {
    const [some, ty1] = instantiateAnnot(term.annot);
    const ty2 = synth(Inst, extend(term.name, ty1, env), term.body);
    if (some.some(t => !isMono(prune(t))))
      return terr(`unannotated parameters used polymorphically in ${showTerm(term)}`);
    return maybeGen(ex, env, TFun(ty1, ty2));    
  }
};

const inferApp = (ex, env, fty, args) => {
  const [tpars, res] = matchfunN(fty, args.length);
  subsumeInferN(env, zip(tpars, args));
  const argsLeft = args.slice(tpars.length);
  if (argsLeft.length === 0)
    return maybeInstOrGen(ex, env, res);
  return inferApp(ex, env, res, argsLeft);
};

const subsumeInferN = (env, tps) => {
  if (tps.length === 0) return;
  const [tpar, arg, rest] = pickArg(tps);
  const targ = synth(isSigma(tpar) ? Gen : Inst, env, arg);
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

module.exports = { infer };
