"use_strict";
if(typeof blake2s1 == 'undefined') blake2s1 = require('./blake2s1.js');

var Candid = (() => {
	var r = {};

	// absolutely minimal unambiguous "pretty" printing
	var pretty = r.pretty = (e) => {
		switch(e.kind){
		case 'star': return '*';
		case 'box': return '□';
		case 'hole': return '_';
		case 'note': return `{- ${e.note} -} ` + pretty(e.body);
		case 'type': return ': ' + pretty(e.type) + ' ' + pretty(e.body);
		case 'hash': return toId(e.hash)
		case 'ref': return e.value.toString();
		case 'rec': return '@' + e.value.toString();
		case 'app': return '$ ' + pretty(e.func) + ' ' + pretty(e.arg);
		case 'pi': return 'π ' + pretty(e.type) + ' ' + pretty(e.body);
		case 'lam': return 'λ ' + pretty(e.type) + ' ' + pretty(e.body);
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
		case 'note': return compile(e.body, depth);
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

	// hash the relevant bits of an expression
	// such that if a portion of the tree is replaced with a hash
	// the overall hash comes out the same
	var _hash = (type, data) => [0,type|0,0,0, 0,0,0,data|0];
	var hash = r.hash = (e) => {
		// cache hashes
		if('hash' in e) return e.hash;
		switch(e.kind) {
		case 'star': e.hash = _hash(-1, 1); break;
		case 'box': e.hash = _hash(-1, -1); break;
		case 'hole': e.hash = _hash(-1, 0); break;
		case 'note': e.hash = hash(e.body); break;
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
		if('closed' in e) return e.closed;
		switch(e.kind){
		case 'star': return -1;
		case 'box': return -1;
		case 'hole': return -1;
		case 'note': return closed(e.body);
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
		case 'note': return hasRec(e.body);
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

	var Box  = r.Box  = ({ kind: 'box' });
	var Hole = r.Hole = ({ kind: 'hole' });
	var Star = r.Star = ({ kind: 'star' });
	var Ref  = r.Ref  = (n) => ({ kind: 'ref', value: n });
	var Rec  = r.Rec  = (n) => ({ kind: 'rec', value: n });
	var Pi   = r.Pi   = (t,b,s,S) => ({ kind: 'pi', type: t, body: b, refname: s, recname: S });
	var Lam  = r.Lam  = (t,b,s,S) => ({ kind: 'lam', type: t, body: b, refname: s, recname: S });
	var App  = r.App  = (f,a) => ({ kind: 'app', func: f, arg: a });
	var Type = r.Type = (t,b) => ({ kind: 'type', type: t, body: b });
	var Note = r.Note = (n,b) => ({ kind: 'note', note: n, body: b });
	var Hash = r.Hash = (h) => ({ kind: 'hash', hash: hash });

	return r;
})();

if(typeof module !== 'undefined' && module.exports)
	module.exports = Candid;
