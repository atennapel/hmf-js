const { resetId, annotAny, prune, isMono, TFun } = require('./types');
const { AAbs, App } = require('./terms');
const { terr } = require('./util');
const { matchfun, subsume } = require('./unify');
const { generalize, instantiateAnnot, instantiate } = require('./operations');

const infer = (env, term) => {
  resetId();
  return prune(synth(env, term));
};

const extend = (n, t, e) => {
  const o = Object.create(e);
  o[n] = t;
  return o;
};

const synth = (env, term) => {
  if (term.tag === 'Var') {
    if (!env[term.name]) return terr(`undefined var ${term.name}`);
    return env[term.name];
  }
  if (term.tag === 'Let') {
    const ty = synth(env, term.val);
    return synth(extend(term.name, ty, env), term.body);
  }
  if (term.tag === 'Abs')
    return synth(env, AAbs(term.name, annotAny, term.body));
  if (term.tag === 'Ann')
    return synth(env, App(AAbs('x', term.annot, Var('x')), term.term));
  if (term.tag === 'Rigid')
    return synth(env, term.term);
  if (term.tag === 'App') {
    const ty1 = synth(env, term.left);
    const { left, right } = matchfun(ty1);
    const ty2 = synth(env, term.right);
    subsume(left, ty2);
    return generalize(env, right);
  }
  if (term.tag === 'AAbs') {
    const [some, ty1] = instantiateAnnot(term.annot);
    const ty2 = synth(extend(term.name, ty1, env), term.body);
    const rho2 = instantiate(ty2);
    if (some.some(t => !isMono(prune(t))))
      return terr(`unannotated parameters used polymorphically in ${showTerm(term)}`);
    return generalize(env, TFun(ty1, rho2));    
  }
};

module.exports = { infer };
