const {
  resetId,
  prune,
  isMono,
  TFun,
  tfunLeft,
  tfunRight,
  freshTMeta,
} = require('./types');
const { terr } = require('./util');
const {
  matchfun,
  subsume,
  instantiate,
  generalize,
} = require('./unify');

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
    return prune(env[term.name]);
  }
  if (term.tag === 'Abs') {
    const tv = freshTMeta();
    const ty = synth(extend(term.name, tv, env), term.body);
    const tp = prune(tv);
    if (!isMono(tp))
      return terr(`infered poly type for abstraction (${showType(tp)}) in ${showTerm(term)}`);
    return generalize(env, TFun(tp, instantiate(ty)));
  }
  if (term.tag === 'App') {
    const ty1 = synth(env, term.left);
    const fun = matchfun(ty1);
    const ty2 = synth(env, term.right);
    subsume(ty2, tfunLeft(fun));
    return generalize(env, tfunRight(fun));
  }
  if (term.tag === 'Ann') {
    check(env, term.term, term.type);
    return prune(term.type);
  }
};

const check = (env, term, type) => {
  const ty = synth(env, term);
  subsume(ty, type);
};

module.exports = {
  infer,
};
