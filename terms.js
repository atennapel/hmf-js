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
}

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
};
