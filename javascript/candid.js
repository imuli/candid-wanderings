"use_strict";
if(typeof blake2s1 == 'undefined') blake2s1 = require('./blake2s1.js');

var Candid = (() => {
	var r = {};

	// absolutely minimal unambiguous "pretty" printing
	var pretty = r.pretty = (e) => {
		var indent = (s) => s.replace(/\n/g, '\n  ');
		switch(e.kind){
		case 'star': return '*';
		case 'hole': return '_';
		case 'type': return ': ' + indent(pretty(e.type)) + indent('\n' + pretty(e.body));
		case 'hash':
				var entry = Candid.fetch(e.hash);
				return entry ? entry.name ? entry.name : pretty(entry.expr) : toId(e.hash);
		case 'ref': return e.value.toString();
		case 'rec': return '@' + e.value.toString();
		case 'app': return '$ ' + indent(pretty(e.func)) + indent('\n' + pretty(e.arg));
		case 'pi': return 'π ' + indent(pretty(e.type)) + ' ' + indent(pretty(e.body));
		case 'lam': return 'λ ' + indent(pretty(e.type)) + indent('\n' + pretty(e.body));
		default: throw "Type Error";
		}
	};

	// compile expression to a string of javascript
	// Note, revisit compiling directly to closures.
	var compile = r.compile = (e, depth) => {
		switch(e.kind) {
		case 'star': return "(()=>$star$)";
		case 'hole': return "(()=>$hole$)";
		case 'type': return compile(e.body, depth);
		case 'hash': return toId(e.hash);
		case 'ref': return `v${depth - e.value}`;
		case 'rec': return `f${depth - e.value}`;
		case 'app': return compile(e.func,depth)+'('+compile(e.arg,depth)+')';
		case 'pi': return "(()=>$pi$)";
		case 'lam':
				if(closed(e) < 0){
					depth = 0;
				}else{
					depth++;
				}
				if(hasRec(e.body, 0)){
					return `(()=>{var f${depth}=(v${depth})=>` +
							compile(e.body,depth) + `;return f${depth};})()`;
				} else {
					return `((v${depth})=>`+compile(e.body,depth)+`)`;
				}
		default: throw "Type Error";
		}
	};

	// reduce expression
	// reduce recursive function if r is specified
	var reduce = r.reduce = (e,r) => {
		// only return a new object if it would be logically different
		var red = e;
		switch(e.kind){
		case 'pi':
				var type = reduce(e.type, r);
				var body = reduce(e.body, r);
				if(!(eq(type, e.type) && eq(body, e.body)))
					red = Pi(type, body, e.argname, e.name);
				break;
		case 'lam':
				var type = reduce(e.type, r);
				var body = reduce(e.body, r);
				if(!(eq(type, e.type) && eq(body, e.body)))
					red = Lam(type, body, e.argname, e.name);
				break;
		case 'app':
				var func = reduce(e.func, r);
				var arg = reduce(e.arg, r);
				// only β-reduce when reducing recursively
				// or there are no recurs to this lambda
				if(func.kind == 'lam' && (r || !hasRec(func.body,0))){
					red = reduce(replace(arg, func, func.body), false);
				}else{
					if(!(eq(func, e.func) && eq(arg, e.arg)))
						red = App(func, arg, e.name);
				}
				break;
		case 'type': // TODO preseve type assertions across β-reduction?
				red = reduce(e.body,r);
				break;
		};
		copynotes(red, e);
		return red;
	};

	// replace references and recurs in an expression
	var replace = (ref, rec, exp) => {
		return shift(-1, over(
			((e,c) => e.value == c ? shift(c+1,ref) : e),
			((e,c) => e.value == c ? shift(c+1,rec) : e), 0, exp));
	};

	// shift unclosed references and recurs in an expression
	var shift = r.shift = (by, exp) => {
		return over(
			((e,c) => e.value >= c ? Ref(e.value+by) : e),
			((e,c) => e.value >= c ? Rec(e.value+by) : e), 0, exp);
	};

	// helper function for shift and replace
	var over = (ref, rec, c, e) => {
		var r = e;
		switch(e.kind){
		case 'ref':  r = ref(e, c); break;
		case 'rec':  r = rec(e, c); break;
		case 'pi':   r = Pi(over(ref,rec,c,e.type), over(ref,rec,c+1,e.body), e.argname, e.name); break;
		case 'lam':  r = Lam(over(ref,rec,c,e.type), over(ref,rec,c+1,e.body), e.argname, e.name); break;
		case 'app':  r = App(over(ref,rec,c,e.func), over(ref,rec,c,e.arg), e.name); break;
		case 'type': r = Type(over(ref,rec,c,e.type), over(ref,rec,c,e.body), e.name); break;
		};
		copynotes(r, e);
		return r;
	};

	// copy notes from e to r (usually it's replacement)
	// like r.note = e.note, but better
	var copynotes = (r,e) => {
		if(r == Star || r.kind == 'hole') return r; // constants don't get notes
		if(r.note == e.note) return r;
		r.note = r.note === undefined ? e.note : e.note === undefined ? r.note : e.note + '\n' + r.note;
		return r;
	};

	// type check expression with parent context
	var typecheck = r.typecheck = (e, ctx) => {
		if(e._type !== undefined) return e._type;
		if(ctx === undefined) ctx = [];
		switch(e.kind) {
		case 'star': return Star;
		case 'hole': return e;
		case 'type': // FIXME how to detect intermediate function application?
				var output_type = e.type;
				for(var i = 0; i < ctx.length; i++){
					switch(ctx[i].kind){
						case 'lam':
							ctx[i].output_type = shift(-i, Pi(ctx[i].type, shift(1, output_type)));
							break;
						case 'pi':
							ctx[i].output_type = Star;
							break;
						default:
							throw 'FIXME - Type assertion within ' + ctx[i].kind;
					}
					output_type = ctx[i].output_type;
				}
				var type = typecheck(e.body, ctx);
				if(!ceq(reduce(unhash(type), true), reduce(unhash(e.type), true), [], []))
					throw { kind: 'Failed Type Assertion', ctx: ctx, et: e.type, at: type };
				e._type = e.type;
				break;
		case 'hash':
				e._type = unhash(fetch(e.hash, true).type);
				break;
		case 'ref':
				if(ctx.length <= e.value)
					throw { kind: 'Open Expression', ctx: ctx, exp: e };
				e._type = shift(e.value+1, ctx[e.value].type);
				break;
		case 'rec':
				if(ctx.length <= e.value)
					throw { kind: 'Open Expression', ctx: ctx, exp: e };
				var rctx = ctx[e.value];
				if(rctx.output_type !== undefined) {
					e._type = shift(e.value, rctx.output_type);
				} else if(rctx.kind === 'pi') {
					e._type = Star;
				} else {
					throw { kind: 'Type Inference', ctx: ctx, exp: e};
				}
				break;
		case 'app':
				var ft = reduce(unhash(typecheck(e.func, ctx)), true);
				if(ft.kind != 'pi'){
					throw { kind: 'Not a Function', ctx: ctx, exp: e, ft: ft };
				}
				var at = typecheck(e.arg, ctx);
				if(!ceq(reduce(unhash(at), true), ft.type, [], [])){
					throw { kind: 'Type Mismatch', ctx: ctx, exp: e, et: ft.type, at: at };
				}
				e._type = reduce(replace(e.arg, ft, ft.body));
				break;
		case 'pi':
				var itt = typecheck(e.type, ctx);
				if(itt.kind != 'star')
					throw { kind: 'Invalid Input Type', ctx: ctx, exp: e, at: itt };
				var ott = typecheck(e.body, [e, ...ctx]);
				if(ott.kind != 'star'){
					throw { kind: 'Invalid Output Type', ctx: ctx, exp: e, at: ott };
				}
				e._type = ott;
				break;
		case 'lam':
				typecheck(e.type, ctx);
				var output_type = typecheck(e.body, [e, ...ctx]);
				e._type = Pi(e.type, output_type, e.argname, undefined); // can't derive a name for the overall pi
				break;
		};
		return e._type;
	};

	// contextual equivalence
	var ceq = (e0, e1, p0, p1) => {
		if(e0.kind == e1.kind) switch(e0.kind){
			case 'type': return ceq(e0.type, e1.type, p0, p1) && ceq(e0.body, e1.body, p0, p1);
			case 'hash': return eq(e0, e1);
			case 'ref':
			case 'rec': return e0.value == e1.value;
			case 'app': return ceq(e0.func, e1.func, p0, p1) && ceq(e0.arg, e1.arg, p0, p1);
			case 'pi':
			case 'lam': return ceq(e0.type, e1.type, p0, p1) && ceq(e0.body, e1.body, [e0].concat(p0), [e1].concat(p1));
			default: return true;
		};
		switch(e0.kind){
			case 'hash': return ceq(unwrap(e0), e1, p0, p1);
			case 'type': return ceq(e0.body, e1, p0, p1);
			case 'app': return ceq(replace(e0.arg, e0.func, e0.func.body), e1, p0, p1);
			case 'rec': return p0.length <= e0.value ? false : ceq(p0[e0.value], e1, p0.slice(e0.value), p1);
		};
		switch(e1.kind){
			case 'hash': return ceq(e0, unwrap(e1), p0, p1);
			case 'type': return ceq(e0, e1.body, p0, p1);
			case 'app': return ceq(e0, replace(e1.arg, e1.func, e1.func.body), p0, p1);
			case 'rec': return p1.length <= e1.value ? false : ceq(e0, p1[e1.value], p0, p1.slice(e1.value));
		};
		return false;
	}

	// logical / hash equivalence
	var eq = (e0, e1) => {
		h0 = hash(e0);
		h1 = hash(e1);
		if(h0 == h1) return true;
		for(var i = 0; i < 8; i++){
			if(h0[i] != h1[i]) return false;
		}
		return true;
	}

	// hash the relevant bits of an expression
	// such that if a portion of the tree is replaced with a hash
	// the overall hash comes out the same
	var zero = r.hash0 = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
	var hash = r.hash = (e) => {
		// cache hashes
		switch(e.kind) {
		case 'star': return blake2s1.hash(zero, [-1,0,0,1],[]);
		case 'hole': return blake2s1.hash(zero, [-1,0,0,0],[]);
		case 'type': return blake2s1.hash(hash(e.type).concat(hash(e.body)),[0,0,1,0],[]);
		case 'hash': return e.hash;
		case 'ref': return blake2s1.hash(zero, [1,0,0,e.value],[]);
		case 'rec': return blake2s1.hash(zero, [2,0,0,e.value],[]);
		case 'app': return blake2s1.hash(hash(e.func).concat(hash(e.arg)),[0,0,0,1],[]);
		case 'pi': return blake2s1.hash(hash(e.type).concat(hash(e.body)),[0,0,0,3],[]);
		case 'lam': return blake2s1.hash(hash(e.type).concat(hash(e.body)),[0,0,0,2],[]);
		default: throw "Type Error";
		}
	};

	// return the depth of πs and λs this expression must be closed within
	var closed = r.closed = (e) => {
		if(e.closed !== undefined) return e.closed;
		switch(e.kind){
		case 'star': return -1;
		case 'hole': return -1;
		case 'type': e.closed = Math.max(closed(e.type), closed(e.body)); return e.closed;
		case 'hash': return -1;
		case 'ref': return e.value;
		case 'rec': return e.value;
		case 'app': e.closed = Math.max(closed(e.func), closed(e.arg)); return e.closed;
		case 'pi':
		case 'lam': e.closed = Math.max(closed(e.type), closed(e.body)-1); return e.closed;
		default: throw "Type Error";
		}
	};

	// look for a recursive call to `depth`
	var hasRec = (e, depth) => {
		switch(e.kind){
		case 'star': return false;
		case 'hole': return false;
		case 'type': return hasRec(e.type, depth) || hasRec(e.body, depth);
		case 'hash': return false;
		case 'ref': return false;
		case 'rec': return e.value == depth;
		case 'app': return hasRec(e.func, depth) || hasRec(e.arg, depth);
		case 'pi':
		case 'lam': return hasRec(e.type, depth) || hasRec(e.body, depth+1);
		default: throw "Type Error";
		}
	};

	// deep clone an expression
	// discarding cached types, etc.
	var clone = r.clone = (expr) => {
		var e = {};
		for(var a in expr){
			if(a[0] == '_') continue;
			if(typeof expr[a] == 'object' && expr[a].kind !== undefined) {
				e[a] = clone(expr[a]);
			} else {
				e[a] = expr[a];
			}
		}
		return e;
	}

	// destructively replace a subexpression identified by `path` with `repl`
	var _update = (expr, path, repl) => {
		// at the end of the path, replace
		if(path.length == 0) return repl;
		expr[path[0]] = _update(expr[path[0]], path.slice(1), repl);
		delete(expr.closed)
		return expr;
	}

	// replace a subexpression identified by `path` with `repl`
	var update = r.update = (expr, path, repl) =>
		_update(clone(expr), path, repl);

	// return the subexpression identified by path
	var byPath = r.byPath = (expr, path) => {
		for(var seg of path)
			expr = expr[seg];
		return expr;
	}

	// expr, type, etc, of closed, fully hashed expressions
	// indexed by hash.
	var _store = r._store = {};

	// save all closed non-trivial sub-expressions to the store
	// this requires type-checking them,
	// and will throw a type error instead of saving ill-typed expressions.
	// returns any wrapping unclosed expression or a hash.
	// replace the topmost expression if replace is set
	var store = r.store = (e, replace) => {
		switch(e.kind){
		case 'star':
		case 'hash':
		case 'ref':
		case 'rec': return e;
		case 'hole': throw 'Attempted to store ' + e.kind + '.';
		case 'type':
				var type = store(e.type);
				var body = unwrap(store(e.body));
				if(type != e.type || body != e.body)
					e = copynotes(Type(type, body, e.name), e);
				replace = true;
				break;
		case 'app':
				var func = store(e.func);
				var arg = store(e.arg);
				if(func != e.func || arg != e.arg)
					e = copynotes(App(func, arg, e.name), e);
				break;
		case 'pi':
				var type = store(e.type);
				var body = store(e.body);
				if(type != e.type || body != e.body)
					e = copynotes(Pi(type, body, e.argname, e.name), e);
				break;
		case 'lam':
				var type = store(e.type);
				var body = store(e.body);
				if(type != e.type || body != e.body)
					e = copynotes(Lam(type, body, e.argname, e.name), e);
				break;
		};
		if(closed(e) >= 0) return e;
		// now that we've reached here, we know:
		// * this expression is closed
		// * it's either an app, pi, or lam
		var type = enhash(typecheck(e));
		// * it's type checked
		var h = hash(e);
		var key = hashToUTF16(h);
		var u = uses(e);
		// so we can safely insert it into our store
		if(replace || _store[key] === undefined){
			_store[key] = {
				expr: e,
				name: e.name,
				type: type,
				hash: h,
				uses: u,
			};
		}
		var s = Hash(h, _store[key].name);
		s._type = type;
		return s;
	};

	// count the uses of other closed expressions
	var uses = r.uses = (e, u) => {
		if(u === undefined) u = {};
		switch(e.kind){
		case 'hash':
				var h = hashToUTF16(e.hash);
				u[h]=u[h] === undefined ? 1 : u[h] + 1;
				var use = fetch(e.hash, true).use;
				for(var h in use)
					u[h] = u[h] === undefined ? use[h] : u[h] + use[h];
				return u;
		case 'app':  return uses(e.func, uses(e.arg, u));
		case 'type':
		case 'pi':
		case 'lam':  return uses(e.type, uses(e.body, u));
		default: return u;
		}
	}

	var remove = r.remove = (e) => {
		var h = hashToUTF16(hash(e));
		if(h in _store){
			for(k in _store){
				if(_store[k] && _store[k].uses[h]) return false;
			}
			_store[h] = undefined;
			return true;
		}
		return undefined;
	}

	// open up all hashes in an expression
	var unhash = r.unhash = (e) => {
		switch(e.kind){
		case 'star':
		case 'ref':
		case 'rec':
		case 'hole':
				break;
		case 'hash':
				e = unhash(unwrap(e));
				break;
		case 'type':
				var type = unhash(e.type);
				var body = unhash(e.body);
				if(type != e.type || body != e.body)
					e = copynotes(Type(type, body, e.name), e);
				break;
		case 'app':
				var func = unhash(e.func);
				var arg = unhash(e.arg);
				if(func != e.func || arg != e.arg)
					e = copynotes(App(func, arg, e.name), e);
				break;
		case 'pi':
				var type = unhash(e.type);
				var body = unhash(e.body);
				if(type != e.type || body != e.body)
					e = copynotes(Pi(type, body, e.argname, e.name), e);
				break;
		case 'lam':
				var type = unhash(e.type);
				var body = unhash(e.body);
				if(type != e.type || body != e.body)
					e = copynotes(Lam(type, body, e.argname, e.name), e);
				break;
		};
		return e;
	};

	// hash the parts of an expression that are in the store
	var enhash = r.enhash = (e) => {
		if(fetch(hash(e)))
			return copynotes(Hash(hash(e), e.name), e);
		switch(e.kind){
		case 'star':
		case 'ref':
		case 'rec':
		case 'hole':
		case 'hash':
				break;
		case 'type':
				var type = enhash(e.type);
				var body = enhash(e.body);
				if(type != e.type || body != e.body)
					e = copynotes(Type(type, body, e.name), e);
				break;
		case 'app':
				var func = enhash(e.func);
				var arg = enhash(e.arg);
				if(func != e.func || arg != e.arg)
					e = copynotes(App(func, arg, e.name), e);
				break;
		case 'pi':
				var type = enhash(e.type);
				var body = enhash(e.body);
				if(type != e.type || body != e.body)
					e = copynotes(Pi(type, body, e.argname, e.name), e);
				break;
		case 'lam':
				var type = enhash(e.type);
				var body = enhash(e.body);
				if(type != e.type || body != e.body)
					e = copynotes(Lam(type, body, e.argname, e.name), e);
				break;
		};
		return e;
	};

	// lookup e in the store, if it's a hash
	var unwrap = r.unwrap = (e) => e.kind == 'hash' ? fetch(e.hash, true).expr : e;
	// fetch an entry for an expression (assert success if required
	var fetch = r.fetch = (h, assert) => {
		var id = hashToUTF16(h);
		var entry = _store[id];
		if(assert && entry === undefined)
			throw id + ' not found in store.';
		return entry;
	};

	var _db = r._db = undefined;

	// open the database
	var open = r.open = () => new Promise((resolve, reject) => {
		var req = window.indexedDB.open("candid", 1);
		req.onerror = (event) => reject(req, event.target.errorCode);
		req.onupgradeneeded = (event) => {
			var db = event.target.result;
			var expr_store = db.createObjectStore("expr", { keyPath: "hash" });
		};
		req.onsuccess = (event) => resolve(req, event);
	}).then((req) => {
		_db = req.result;
	});

	// save to the database
	var save = r.save = () => new Promise((resolve, reject) => {
		if(_db === undefined) return open().then(load);
		var es = _db.transaction(["expr"], "readwrite").objectStore("expr");
		var i = 0, keys = Object.keys(_store);
		var go = () => {
			if(i >= keys.length) return resolve();
			var p;
			if(_store[keys[i]] === undefined){
				p = es.delete(hashFromUTF16(keys[i], {offset:0}));
				delete _store[keys[i]];
			} else {
				p = es.put(_store[keys[i]]);
			}
			i++;
			p.onsuccess = go;
			p.onerror = reject;
		};
		return go();
	});

	// load from the database
	var load = r.load = () => new Promise((resolve, reject) => {
		if(_db === undefined) return open().then(load).then(resolve, reject);
		var es = _db.transaction(["expr"]).objectStore("expr");
		var req = es.getAll();
		req.onsuccess = () => {
			var exprs = req.result;
			console.log(exprs);
			for(var i in exprs) {
				_store[hashToUTF16(exprs[i].hash)] = exprs[i];
			}
			resolve();
		}
		req.onerror = reject;
	});

	// render to compact UTF16 format
	// suitable for use with localStorage
	var toUTF16 = r.toUTF16 = (e) => {
		switch(e.kind){
		case 'star': return '*';
		case 'hole': return '_';
		case 'type': return ':' + toUTF16(e.type) + toUTF16(e.body);
		case 'hash': return '#' + hashToUTF16(e.hash)
		case 'ref':  return '!' + natToUTF16(e.value);
		case 'rec':  return '@' + natToUTF16(e.value);
		case 'app':  return '$' + toUTF16(e.func) + toUTF16(e.arg);
		case 'pi':   return 'π' + toUTF16(e.type) + toUTF16(e.body);
		case 'lam':  return 'λ' + toUTF16(e.type) + toUTF16(e.body);
		default: throw "Type Error";
		};
	};

	// parse an expression from the format above
	var fromUTF16 = r.fromUTF16 = (s, st) => {
		if(st === undefined) st = {offset: 0};
		var c = s[st.offset];
		st.offset++;
		switch(c){
		case '*': return Star;
		case '_': return Hole();
		case ':': return Type(fromUTF16(s,st), fromUTF16(s,st));
		case '#': return Hash(hashFromUTF16(s, st));
		case '!': return Ref(natFromUTF16(s, st));
		case '@': return Rec(natFromUTF16(s, st));
		case '$': return App(fromUTF16(s,st),fromUTF16(s,st));
		case 'π': return Pi(fromUTF16(s,st),fromUTF16(s,st));
		case 'λ': return Lam(fromUTF16(s,st),fromUTF16(s,st));
		default: throw s + "\nParse Error at " + st.offset + " recieved " + c;
		};
	};

	// convert u32 to 0..4 unicode points [0x200,0x2ff]
	var natToUTF16 = (n) => {
		var a = [];
		while(n > 0){
			a.push(n & 0xff | 0x200);
			n = n >> 8;
		}
		return String.fromCharCode(...a);
	}

	// parse u32 from 0..4 unicode points [0x200,0x2ff]
	var natFromUTF16 = (s, st) => {
		var n = 0;
		s = s.substr(st.offset);
		var i;
		for(i = 0; i < 4 && (c = s.charCodeAt(i)) && (0x200 <= c) && (c <= 0x2ff); i++){
			n |= (c & 0xff) << (8*i);
		}
		st.offset += i;
		return n;
	}

	// render hash as unicode points [0x100,0x1ff]
	var hashToUTF16 = (h) => {
		var a = [];
		for(var i = 0; i < 8; i++){
			for(var j = 0; j < 4; j++){
				a.push((h[i] >>> (8*j)) & 0xff | 0x100);
			}
		}
		return String.fromCharCode(...a);
	};

	// parse hash from unicode points [0x100,0x1ff]
	var hashFromUTF16 = (_s, st) => {
		var s = _s.substr(st.offset);
		var h = [];
		for(var i = 0; i < 8; i++){
			var x = 0;
			for(var j = 0; j < 4; j++){
				var c = s.charCodeAt(4*i+j);
				if(c < 0x100 || 0x1ff < c) throw _s + "\nParse error at 7, recieved " + c;
				x |= (s.charCodeAt(4*i+j) & 0xff) << (8*j);
			}
			h.push(x);
		}
		st.offset += 32;
		return h;
	};

	// render hash as an ascii javascript identifier
	var toId = r.toId = function(a) {
		if(!a) a = this;
		var b = [
			'a','b','c','d','e','f','g','h',
			'i','j','k','l','m','n','o','p',
			'q','r','s','t','u','v','w','x',
			'y','z','A','B','C','D','E','F',
			'G','H','I','J','K','L','M','N',
			'O','P','Q','R','S','T','U','V',
			'W','X','Y','Z','_','$','0','1',
			'2','3','4','5','6','7','8','9',
		];
		var x = [0]; // pad the beginning to ensure this doesn't start with a number
		for(var i = 0; i < 8; i++){
			for(var j = 0; j < 4; j++){
				// big endian bytes in little endian words
				x.push((a[i] >>> (8*j+6)) & 0x3 );
				x.push((a[i] >>> (8*j+4)) & 0x3 );
				x.push((a[i] >>> (8*j+2)) & 0x3 );
				x.push((a[i] >>> (8*j+0)) & 0x3 );
			}
		}
		var s = "";
		for(var i = 0; i < 43; i++){
			var j = x[3*i+0] << 4 | x[3*i+1] << 2 | x[3*i+2] << 0;
			s += b[j];
		}
		return s;
	};

	// helper functions (and objects) for building expressions.
	// these are not the only way to build expressions!
	// and do not perscribe which values may take which arguments
	// in particular, all values will eventually have `hash` may have a `note`
	var Hole = r.Hole = (h) => ({ kind: 'hole', hold: h });
	var Star = r.Star = ({ kind: 'star' });
	var Ref  = r.Ref  = (n) => ({ kind: 'ref', value: n });
	var Rec  = r.Rec  = (n) => ({ kind: 'rec', value: n });
	var Pi   = r.Pi   = (t,b,an,n) => ({ kind: 'pi', type: t, body: b, argname: an, name: n});
	var Lam  = r.Lam  = (t,b,an,n) => ({ kind: 'lam', type: t, body: b, argname: an, name: n });
	var App  = r.App  = (f,a,n) => ({ kind: 'app', func: f, arg: a, name: n });
	var Apps = r.Apps = (f,...as) => as.reduce((f,a) => ({ kind: 'app', func: f, arg: a }), f);
	var Type = r.Type = (t,b,n) => ({ kind: 'type', type: t, body: b, name: n });
	var Hash = r.Hash = (h,n) => ({ kind: 'hash', hash: h, name: n });

	return r;
})();

if(typeof module !== 'undefined' && module.exports)
	module.exports = Candid;
