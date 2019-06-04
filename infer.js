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
const { Nil, Cons, extend, lookup, toString } = require('./list');

const infer = (env, term) => {
  resetId();
  return prune(synth(env, Nil, term, Nil));
};

const synth = (env, lenv, term, args) => {
  console.log(`synth ${showTerm(term)} - ${toString(args, showTerm)}`);
  if (term.tag === 'Var') {
    const ty = lookup(lenv, term.name) || env[term.name];
    if (!ty) return terr(`undefined var ${term.name}`);
    return prune(ty);
  }
  if (term.tag === 'Abs') {
    if (term.type) {
      const ty = synth(env, extend(term.name, term.type, lenv), term.body, Nil);
      const rty = term.body.tag === 'Ann' ? ty : instantiate(ty);
      return generalize(env, TFun(term.type, rty));
    } else {
      const tv = freshTMeta();
      const ty = synth(env, extend(term.name, tv, lenv), term.body, Nil);
      const tp = prune(tv);
      if (!isMono(tp))
        return terr(`infered poly type for abstraction (${showType(tp)}) in ${showTerm(term)}`);
      const rty = term.body.tag === 'Ann' ? ty : instantiate(ty);
      return generalize(env, TFun(tp, rty));
    }
  }
  if (term.tag === 'App') {
    const ty1 = synth(env, lenv, term.left, Cons(term.right, args));
    const fun = matchfun(ty1);
    check(env, lenv, term.right, tfunLeft(fun), Nil);
    return generalize(env, tfunRight(fun));
  }
  if (term.tag === 'Ann') {
    check(env, lenv, term.term, term.type, args);
    return prune(term.type);
  }
};

const check = (env, lenv, term, type, args) => {
  console.log(`check ${showTerm(term)} : ${showType(type)}`);
  if (term.tag === 'Abs') {
    const tf = matchfun(type);
    if (term.type) unify(tfunLeft(tf), term.type);
    check(env, extend(term.name, tfunLeft(tf), lenv), term.body, tfunRight(tf), Nil);
    return;
  }
  if (term.tag === 'App') {
    const ty1 = synth(env, lenv, term.left, Cons(term.right, args));
    const fun = matchfun(ty1);
    subsume(tfunRight(fun), type);
    check(env, lenv, term.right, tfunLeft(fun), Nil);
    return generalize(env, tfunRight(fun));
  }
  const ty = synth(env, lenv, term, args);
  if (term.tag === 'Ann')
    unify(ty, type);
  else
    subsume(ty, type);
};

module.exports = {
  infer,
};
