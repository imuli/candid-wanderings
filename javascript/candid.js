"use_strict";
if(typeof blake2s1 == 'undefined') blake2s1 = require('./blake2s1.js');

var Candid = (() => {
	var r = {};

	// absolutely minimal unambiguous "pretty" printing
	var pretty = r.pretty = (e) => {
		var indent = (s) => s.replace(/\n/g, '\n  ');
		switch(e.kind){
		case 'star': return '*';
		case 'box': return '□';
		case 'hole': return '_';
		case 'type': return ': ' + indent(pretty(e.type)) + indent('\n' + pretty(e.body));
		case 'hash': return toId(e.hash)
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
		case 'box': return "(()=>$box$)";
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
					red = Pi(type, body, argname, e.name);
				break;
		case 'lam':
				var type = reduce(e.type, r);
				var body = reduce(e.body, r);
				if(!(eq(type, e.type) && eq(body, e.body)))
					red = Lam(type, body, e.argname, e.name);
				break;
		case 'app':
				var func = reduce(e.func, r);
				// reducing arguments recursively causes infinite loops
				var arg = reduce(e.arg, false);
				// only β-reduce when reducing recursively
				// or there are no recurs to this lambda
				if(func.kind == 'lam' && (r || !hasRec(func.body,0))){
					red = reduce(replace(arg, func, func.body), r);
				}else{
					if(!(eq(func, e.func) && eq(arg, e.arg)))
						red = App(func, arg);
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
	var shift = (by, exp) => {
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
		case 'app':  r = App(over(ref,rec,c,e.func), over(ref,rec,c,e.arg)); break;
		case 'type': r = Type(over(ref,rec,c,e.type), over(ref,rec,c,e.body)); break;
		};
		copynotes(r, e);
		return r;
	};

	// copy notes from e to r (usually it's replacement)
	// like r.note = e.note, but better
	var copynotes = (r,e) => {
		if(r == Star || r == Box || r == Hole) return r; // constants don't get notes
		if(r.note == e.note) return r;
		r.note = r.note === undefined ? e.note : e.note === undefined ? r.note : e.note + '\n' + r.note;
		return r;
	};

	// type check expression with parent context
	var typecheck = r.typecheck = (e, ctx) => {
		if(e._type !== undefined) return e._type;
		if(ctx === undefined) ctx = [];
		switch(e.kind) {
		case 'star': return Box;
		case 'box': throw { kind: 'Untyped Box', ctx: ctx};
		case 'hole': return Hole;
		case 'type': // FIXME how to detect intermediate function application?
				var output_type = e.type;
				for(var i = 0; i < ctx.length; i++){
					if(ctx[i].exp.kind == 'lam'){
						ctx[i].output_type = Pi(ctx[i].exp.type, shift(1, output_type));
					} else {
						throw 'FIXME - Type assertion within ' + ctx[i].exp.kind;
					}
					output_type = ctx[i].output_type;
				}
				var type = typecheck(e.body, ctx);
				if(!ceq(type, e.type, [], []))
					throw { kind: 'Failed Type Assertion', ctx: ctx, claim: claim, type: type };
				e._type = e.type;
				break;
		case 'hash':
				e._type = unwrap(fetch(e.hash, true).type);
				break;
		case 'ref':
				if(ctx.length <= e.value)
					throw { kind: 'Open Expression', ctx: ctx, exp: e };
				e._type = ctx[e.value].input_type;
				break;
		case 'rec':
				if(ctx.length <= e.value)
					throw { kind: 'Open Expression', ctx: ctx, exp: e };
				var rctx = ctx[e.value];
				if(rctx.output_type !== undefined) {
					e._type = rctx.output_type;
				} else if(rctx.exp.kind === 'pi') {
					e._type = Star;
				} else {
					throw { kind: 'Type Inference', ctx: ctx, exp: e};
				}
				break;
		case 'app':
				var ft = unwrap(typecheck(e.func, ctx));
				if(ft.kind != 'pi'){
					throw { kind: 'Not a Function', ctx: ctx, exp: e, ft: ft };
				}
				var at = typecheck(e.arg, ctx);
				if(!ceq(at, ft.type, [], [])){
					throw { kind: 'Type Mismatch', ctx: ctx, exp: e, at: at };
				}
				e._type = reduce(replace(e.arg, ft, ft.body), true);
				break;
		case 'pi':
				var itt = typecheck(e.type, ctx);
				if(itt.kind != 'star' && itt.kind != 'box')
					throw { kind: 'Invalid Input Type', ctx: ctx, exp: e, type: itt };
				var ott = typecheck(e.body, extend(ctx, e.type, e));
				if(ott.kind != 'star' && ott.kind != 'box'){
					throw { kind: 'Invalid Output Type', ctx: ctx, exp: e, type: ott };
				}
				e._type = ott;
				break;
		case 'lam':
				typecheck(e.type, ctx);
				var output_type = typecheck(e.body, extend(ctx, e.type, e));
				e._type = Pi(e.type, output_type, e.argname, undefined); // can't derive a name for the overall pi
				break;
		};
		return e._type;
	};

	// extend a type checking context
	var extend = (ctx, type, exp) => {
		var newctx = ctx.map((e) => ({
			exp: e.exp,
			input_type: shift(1, e.input_type),
			output_type: e.output_type === undefined ? undefined : shift(1, e.output_type),
		}));
		newctx.unshift({
			exp: exp,
			input_type: shift(1, type),
		});
		return newctx;
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
			case 'rec': return p0.length <= e0.value ? false : ceq(p0[e0.value], e1, p0.slice(e0.value), p1);
		};
		switch(e1.kind){
			case 'hash': return ceq(e0, unwrap(e1), p0, p1);
			case 'type': return ceq(e0, e1.body, p0, p1);
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
	var _hash = (type, data) => [0,type|0,0,0, 0,0,0,data|0];
	var hash = r.hash = (e) => {
		// cache hashes
		if(e.hash !== undefined) return e.hash;
		switch(e.kind) {
		case 'star': e.hash = _hash(-1, 1); break;
		case 'box': e.hash = _hash(-1, -1); break;
		case 'hole': e.hash = _hash(-1, 0); break;
		case 'type': e.hash = hash(e.body); break;
		case 'hash': e.hash = e.hash; break; // never get here anyway
		case 'ref': e.hash = _hash(1, e.value); break;
		case 'rec': e.hash = _hash(2, e.value); break;
		case 'app': e.hash = blake2s1.hash(hash(e.func).concat(hash(e.arg)),[0,0,0,1],[]); break;
		case 'pi': e.hash = blake2s1.hash(hash(e.type).concat(hash(e.body)),[0,0,0,3],[]); break;
		case 'lam': e.hash = blake2s1.hash(hash(e.type).concat(hash(e.body)),[0,0,0,2],[]); break;
		default: throw "Type Error";
		}
		e.hash.toString = toId;
		return e.hash;
	};

	// return the depth of πs and λs this expression must be closed within
	var closed = r.closed = (e) => {
		if(e.closed !== undefined) return e.closed;
		switch(e.kind){
		case 'star': return -1;
		case 'box': return -1;
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
		case 'box': return false;
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

	// expr, type, etc, of closed, fully hashed expressions
	// indexed by hash.
	var _store = {};

	// save all closed non-trivial sub-expressions to the store
	// this requires type-checking them,
	// and will throw a type error instead of saving ill-typed expressions.
	// returns any wrapping unclosed expression or a hash.
	var store = r.store = (e) => {
		switch(e.kind){
		case 'star':
		case 'hash':
		case 'ref':
		case 'rec': return e;
		case 'box':
		case 'hole': throw 'Attempted to store ' + e.kind + '.';
		case 'type':
				var type = store(e.type);
				var body = store(e.body);
				if(type != e.type || body != e.body)
					e = Type(type, body, e.note);
				return e;
		case 'app':
				var func = store(e.func);
				var arg = store(e.arg);
				if(func != e.func || arg != e.arg)
					e = copynotes(App(func, arg), e);
				if(func.kind != 'hash' || arg.kind != 'hash')
					return e;
				break;
		case 'pi':
				var type = store(e.type);
				var body = store(e.body);
				if(type != e.type || body != e.body)
					e = copynotes(Pi(type, body, e.argname, e.name), e);
				if(closed(type) >= 0 || closed(body) >= 1)
					return e;
				break;
		case 'lam':
				var type = store(e.type);
				var body = store(e.body);
				if(type != e.type || body != e.body)
					e = copynotes(Lam(type, body, e.argname, e.name), e);
				if(closed(type) >= 0 || closed(body) >= 1)
					return e;
				break;
		};
		// now that we've reached here, we know:
		// * this expression is closed
		// * it's either an app, pi, or lam
		var type = typecheck(e);
		// * it's type checked
		var h = hash(e);
		var key = toId(h);
		// so we can safely insert it into our store
		if(_store[key] === undefined){
			_store[key] = {
				expr: e,
				type: type,
				hash: h,
			};
		}
		var s = Hash(h, e.name);
		s._type = type;
		return s;
	};

	// lookup e in the store, if it's a hash
	var unwrap = r.unwrap = (e) => e.kind == 'hash' ? fetch(e.hash, true).expr : e;
	// fetch an entry for an expression (assert success if required
	var fetch = r.fetch = (h, assert) => {
		var id = toId(h);
		var entry = _store[id];
		if(assert && entry === undefined)
			throw id + ' not found in store.';
		return entry;
	};

	// render hash as an ascii javascript identifier
	var toId = function(a) {
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
	var Box  = r.Box  = ({ kind: 'box' });
	var Hole = r.Hole = ({ kind: 'hole' });
	var Star = r.Star = ({ kind: 'star' });
	var Ref  = r.Ref  = (n) => ({ kind: 'ref', value: n });
	var Rec  = r.Rec  = (n) => ({ kind: 'rec', value: n });
	var Pi   = r.Pi   = (t,b,an,n) => ({ kind: 'pi', type: t, body: b, argname: an, name: n});
	var Lam  = r.Lam  = (t,b,an,n) => ({ kind: 'lam', type: t, body: b, argname: an, name: n });
	var App  = r.App  = (f,...as) => as.reduce((f,a) => ({ kind: 'app', func: f, arg: a }), f);
	var Type = r.Type = (t,b,n) => ({ kind: 'type', type: t, body: b, note: n });
	var Hash = r.Hash = (h,n) => ({ kind: 'hash', hash: h, name: n });

	return r;
})();

if(typeof module !== 'undefined' && module.exports)
	module.exports = Candid;
