const { showAnnot } = require('./types');

const Var = name => ({ tag: 'Var', name });
const App = (left, right) => ({ tag: 'App', left, right });
const Abs = (name, body) => ({ tag: 'Abs', name, body });
const AAbs = (name, annot, body) =>
  ({ tag: 'AAbs', name, annot, body });
const Let = (name, val, body) =>
  ({ tag: 'Let', name, val, body });
const Ann = (term, annot) => ({ tag: 'Ann', term, annot });
const Rigid = term => ({ tag: 'Rigid', term });

const appFrom = ts => ts.reduce(App);
function app() { return appFrom(Array.from(arguments)) }

const abs = (ns, body) => ns.reduceRight((b, n) => Abs(n, b), body);
const aabs = (ns, body) =>
  ns.reduceRight((b, [n, t]) => AAbs(n, t, b), body);

const lets = (vals, body) =>
  vals.reduceRight((b, [n, t]) => Let(n, t, b), body);

const isAnnot = t =>
  (t.tag === 'Let' && isAnnot(t.body)) ||
  t.tag === 'Rigid' || t.tag === 'Ann';

const showTerm = t => {
  if (t.tag === 'Var') return t.name;
  if (t.tag === 'App')
    return `(${showTerm(t.left)} ${showTerm(t.right)})`;
  if (t.tag === 'Abs') return `(\\${t.name} -> ${showTerm(t.body)})`;
  if (t.tag === 'AAbs')
    return `(\\(${t.name} : ${showAnnot(t.annot)}) -> ${showTerm(t.body)})`;
  if (t.tag === 'Let')
    return `(let ${t.name} = ${showTerm(t.val)} in ${showTerm(t.body)})`;
  if (t.tag === 'Ann')
    return `(${showTerm(t.term)} : ${showAnnot(t.annot)})`;
  if (t.tag === 'Rigid')
    return `(rigid ${showTerm(t.term)})`;
};

const flattenApp = t => {
  const args = [];
  let c = t;
  while (c.tag === 'App') {
    args.push(c.right);
    c = c.left;
  }
  return [c, args.reverse()];
};

module.exports = {
  Var,
  App,
  Abs,
  AAbs,
  Let,
  Ann,
  Rigid,
  isAnnot,
  showTerm,
  flattenApp,
  app,
  appFrom,
  abs,
  aabs,
  lets,
};
