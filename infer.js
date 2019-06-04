const {
  resetId,
  prune,
  isMono,
  TFun,
  isTFun,
  tfunLeft,
  tfunRight,
  freshTMeta,
  showType,
} = require('./types');
const { terr } = require('./util');
const {
  matchfun,
  subsume,
  unify,
  instantiate,
  generalize,
} = require('./unify');
const { showTerm } = require('./terms');

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
  console.log(`synth ${showTerm(term)}`);
  if (term.tag === 'Var') {
    if (!env[term.name]) return terr(`undefined var ${term.name}`);
    return prune(env[term.name]);
  }
  if (term.tag === 'Abs') {
    if (term.type) {
      const ty = synth(extend(term.name, term.type, env), term.body);
      return generalize(env, TFun(term.type, instantiate(ty)));
    } else {
      const tv = freshTMeta();
      const ty = synth(extend(term.name, tv, env), term.body);
      const tp = prune(tv);
      if (!isMono(tp))
        return terr(`infered poly type for abstraction (${showType(tp)}) in ${showTerm(term)}`);
      return generalize(env, TFun(tp, instantiate(ty)));
    }
  }
  if (term.tag === 'App') {
    const ty1 = synth(env, term.left);
    const fun = matchfun(ty1);
    check(env, term.right, tfunLeft(fun));
    return generalize(env, tfunRight(fun));
  }
  if (term.tag === 'Ann') {
    check(env, term.term, term.type);
    return prune(term.type);
  }
};

const check = (env, term, type) => {
  console.log(`check ${showTerm(term)} : ${showType(type)}`);
  if (term.tag === 'Abs' && isTFun(type)) {
    if (term.type) unify(tfunLeft(type), term.type);
    check(extend(term.name, tfunLeft(type), env), term.body, tfunRight(type));
    return;
  }
  if (term.tag === 'App') {
    const ty1 = synth(env, term.left);
    const fun = matchfun(ty1);
    subsume(tfunRight(fun), type);
    check(env, term.right, tfunLeft(fun));
    return generalize(env, tfunRight(fun));
  }
  const ty = synth(env, term);
  subsume(ty, type);
};

module.exports = {
  infer,
};
