const Nil = { tag: 'Nil' };
const Cons = (head, tail) => ({ tag: 'Cons', head, tail });

const toString = (l, fn = x => `${x}`) => {
  const r = [];
  let c = l;
  while (c.tag === 'Cons') {
    r.push(fn(c.head));
    c = c.tail;
  }
  return `[${r.join(', ')}]`;
};

const filter = (l, fn) =>
  l.tag === 'Cons' ? (fn(l.head) ? Cons(l.head, filter(l.tail, fn)) : filter(l.tail, fn)) : l;
const first = (l, fn) => {
  let c = l;
  while (c.tag === 'Cons') {
    if (fn(c.head)) return c.head;
    c = c.tail;
  }
  return null;
};
const each = (l, fn) => {
  let c = l;
  while (c.tag === 'Cons') {
    fn(c.head);
    c = c.tail;
  }
};

const toArray = (l, fn) => {
  let c = l;
  const r = [];
  while (c.tag === 'Cons') {
    r.push(fn(c.head));
    c = c.tail;
  }
  return r;
};

const append = (a, b) =>
  a.tag === 'Cons' ? Cons(a.head, append(a.tail, b)) : b;

const map = (l, fn) =>
  l.tag === 'Cons' ? Cons(fn(l.head), map(l.tail, fn)) : l;

const index = (l, i) => {
  while (l.tag === 'Cons') {
    if (i-- === 0) return l.head;
    l = l.tail;
  }
  return null;
};

const extend = (name, val, rest) =>
  Cons([name, val], rest);
const lookup = (l, name) => {
  while (l.tag === 'Cons') {
    const h = l.head;
    if (h[0] === name) return h[1];
    l = l.tail;
  }
  return null;
};

module.exports = {
  Nil,
  Cons,
  toString,
  filter,
  first,
  each,
  toArray,
  append,
  map,
  index,
  extend,
  lookup,
};
