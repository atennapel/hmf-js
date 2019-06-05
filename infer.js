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
const { showTerm, flattenApp } = require('./terms');
const { Nil, extend, lookup } = require('./list');

const infer = (env, term) => {
  resetId();
  return prune(synth(env, Nil, term));
};

const synth = (env, lenv, term) => {
  console.log(`synth ${showTerm(term)}`);
  if (term.tag === 'Var') {
    const ty = lookup(lenv, term.name) || env[term.name];
    if (!ty) return terr(`undefined var ${term.name}`);
    return prune(ty);
  }
  if (term.tag === 'Abs') {
    if (term.type) {
      const ty = synth(env, extend(term.name, term.type, lenv), term.body);
      const rty = term.body.tag === 'Ann' ? ty : instantiate(ty);
      return generalize(env, TFun(term.type, rty));
    } else {
      const tv = freshTMeta();
      const ty = synth(env, extend(term.name, tv, lenv), term.body);
      const tp = prune(tv);
      if (!isMono(tp))
        return terr(`infered poly type for abstraction (${showType(tp)}) in ${showTerm(term)}`);
      const rty = term.body.tag === 'Ann' ? ty : instantiate(ty);
      return generalize(env, TFun(tp, rty));
    }
  }
  if (term.tag === 'Ann') {
    check(env, lenv, term.term, term.type);
    return prune(term.type);
  }
  if (term.tag === 'App') {
    const [fn, args] = flattenApp(term);
    const ft = synth(env, lenv, fn);
    console.log(`${showType(ft)} @ [${args.map(showTerm).join(', ')}]`);
    return terr('unimplemented app');
  }
};

const check = (env, lenv, term, type) => {
  console.log(`check ${showTerm(term)} : ${showType(type)}`);
  if (term.tag === 'Abs') {
    const tf = matchfun(type);
    if (term.type) unify(tfunLeft(tf), term.type);
    check(env, extend(term.name, tfunLeft(tf), lenv), term.body, tfunRight(tf));
    return;
  }
  const ty = synth(env, lenv, term);
  if (term.tag === 'Ann')
    unify(ty, type);
  else
    subsume(ty, type);
};

module.exports = {
  infer,
};
