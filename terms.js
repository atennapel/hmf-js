const { showType } = require('./types');

const Var = name => ({ tag: 'Var', name });
const App = (left, right) => ({ tag: 'App', left, right });
const Abs = (name, body, type = null) => ({ tag: 'Abs', name, type, body });
const Ann = (term, type) => ({ tag: 'Ann', term, type });

const appFrom = ts => ts.reduce(App);
function app() { return appFrom(Array.from(arguments)) }
const abs = (ns, body) => ns.reduceRight((b, n) => Abs(n, b), body);

const showTerm = t => {
  if (t.tag === 'Var') return `${t.name}`;
  if (t.tag === 'App')
    return `(${showTerm(t.left)} ${showTerm(t.right)})`;
  if (t.tag === 'Abs')
    return t.type ?
    ` (\\(${t.name} : ${showType(t.type)}) -> ${showTerm(t.body)})` :
      `(\\${t.name} -> ${showTerm(t.body)})`;
  if (t.tag === 'Ann')
    return `(${showTerm(t.term)} : ${showType(t.type)})`;
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
  Ann,
  appFrom,
  app,
  abs,
  showTerm,
  flattenApp,
};
