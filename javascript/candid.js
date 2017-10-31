"use_strict";
if(typeof blake2s1 == 'undefined') blake2s1 = require('./blake2s1.js');

var Candid = {
	// absolutely minimal unambiguous "pretty" printing
	pretty : function(e) {
		if(!e) e = this;
		switch(e.kind) {
		case 'star': return '*';
		case 'box': return '□';
		case 'hole': return '_';
		case 'note': return `{- ${e.note} -} ` + Candid.pretty(e.body);
		case 'type': return ': ' + Candid.pretty(e.type) + ' ' + Candid.pretty(e.body);
		case 'hash': return Candid.toId(e.hash)
		case 'ref': return e.value.toString();
		case 'rec': return '@' + e.value.toString();
		case 'app': return '$ ' + Candid.pretty(e.func) + ' ' + Candid.pretty(e.arg);
		case 'pi': return 'π ' + Candid.pretty(e.type) + ' ' + Candid.pretty(e.body);
		case 'lam': return 'λ ' + Candid.pretty(e.type) + ' ' + Candid.pretty(e.body);
		default: throw "Type Error";
		}
	},
	// compile expression to a string of javascript
	// Note, revisit compiling directly to closures.
	compile : (e, depth) => {
		switch(e.kind) {
		case 'star': return "(()=>$star$)";
		case 'box': return "(()=>$box$)";
		case 'hole': return "(()=>$hole$)";
		case 'note': return Candid.compile(e.body, depth);
		case 'type': return Candid.compile(e.body, depth);
		case 'hash': return Candid.toId(e.hash);
		case 'ref': return `v${depth - e.value}`;
		case 'rec': return `f${depth - e.value}`;
		case 'app': return Candid.compile(e.func,depth)+'('+Candid.compile(e.arg,depth)+')';
		case 'pi': return "(()=>$pi$)";
		case 'lam':
				if(Candid.closed(e) < 0){
					depth = 0;
				}else{
					depth++;
				}
				if(Candid.hasRec(e.body, 0)){
					return `(()=>{var f${depth}=(v${depth})=>` +
							Candid.compile(e.body,depth) + `;return f${depth};})()`;
				} else {
					return `((v${depth})=>`+Candid.compile(e.body,depth)+')';
				}
		default: throw "Type Error";
		}
	},
	// hash the relevant bits of an expression
	// such that if a portion of the tree is replaced with a hash
	// the overall hash comes out the same
	_hash : (type, data) => [0,type|0,0,0, 0,0,0,data|0],
	hash : (e) => {
		// cache hashes
		if('hash' in e) return e.hash;
		switch(e.kind) {
		case 'star': e.hash = Candid._hash(-1, 1); break;
		case 'box': e.hash = Candid._hash(-1, -1); break;
		case 'hole': e.hash = Candid._hash(-1, 0); break;
		case 'note': e.hash = Candid.hash(e.body); break;
		case 'type': e.hash = Candid.hash(e.body); break;
		case 'hash': e.hash = e.hash; break; // never get here anyway
		case 'ref': e.hash = Candid._hash(1, e.value); break;
		case 'rec': e.hash = Candid._hash(2, e.value); break;
		case 'app': e.hash = blake2s1.hash(Candid.hash(e.func).concat(Candid.hash(e.arg)), [0,0,0,1], []); break;
		case 'pi': e.hash = blake2s1.hash(Candid.hash(e.type).concat(Candid.hash(e.body)), [0,0,0,3], []); break;
		case 'lam': e.hash = blake2s1.hash(Candid.hash(e.type).concat(Candid.hash(e.body)), [0,0,0,2], []); break;
		default: throw "Type Error";
		}
		e.hash.toString = Candid.toId;
		return e.hash;
	},
	// return the depth of πs and λs this expression must be closed within
	closed : (e) => {
		if('closed' in e) return e.closed;
		switch(e.kind){
		case 'star': return -1;
		case 'box': return -1;
		case 'hole': return -1;
		case 'note': return Candid.closed(e.body);
		case 'type': e.closed = Math.max(Candid.closed(e.type), Candid.closed(e.body)); return e.closed;
		case 'hash': return -1;
		case 'ref': return e.value;
		case 'rec': return e.value;
		case 'app': e.closed = Math.max(Candid.closed(e.func), Candid.closed(e.arg)); return e.closed;
		case 'pi':
		case 'lam': e.closed = Math.max(Candid.closed(e.type), Candid.closed(e.body)-1); return e.closed;
		default: throw "Type Error";
		}
	},
	// look for a recursive call to `depth`
	hasRec : (e, depth) => {
		switch(e.kind){
		case 'star': return false;
		case 'box': return false;
		case 'hole': return false;
		case 'note': return Candid.hasRec(e.body);
		case 'type': return Candid.hasRec(e.type, depth) || Candid.hasRec(e.body, depth);
		case 'hash': return false;
		case 'ref': return false;
		case 'rec': return e.value == depth;
		case 'app': return Candid.hasRec(e.func, depth) || Candid.hasRec(e.arg, depth);
		case 'pi':
		case 'lam': return Candid.hasRec(e.type, depth) || Candid.hasRec(e.body, depth+1);
		default: throw "Type Error";
		}
	},
	// render hash as an ascii javascript identifier
	toId : function(a) {
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
	},
};

if(typeof module !== 'undefined' && module.exports)
	module.exports = Candid;
