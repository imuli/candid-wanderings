"use_strict";
// requires Candid and Inferno to be loaded first

var E = Inferno.createElement;

// subtract each Rec and Ref from size
// to give consistant colors within encapulating closed expression
var juggle = (expr, size) => {
	if(Candid.closed(expr) < 0) return expr;
	switch(expr.kind){
		case 'ref': return Candid.Ref(size - expr.value);
		case 'rec': return Candid.Rec(size - expr.value);
		case 'type':
			var type = juggle(expr.type, size),
				body = juggle(expr.body, size);
			return type != expr.type || body != expr.body ? Candid.Type(type, body) : expr;
		case 'app':
			var func = juggle(expr.func, size),
				arg = juggle(expr.arg, size);
			return func != expr.func || arg != expr.arg ? Candid.App(func, arg) : expr;
		case 'pi':
			var type = juggle(expr.type, size),
				body = juggle(expr.body, size);
			return type != expr.type || body != expr.body ? Candid.Pi(type, body) : expr;
		case 'lam':
			var type = juggle(expr.type, size),
				body = juggle(expr.body, size);
			return type != expr.type || body != expr.body ? Candid.Lam(type, body) : expr;
		default: return expr;
	}
}

var colorExpr = (expr, ctx) => {
	if(expr === undefined) return undefined;
	expr = juggle(expr, ctx.length);
	var c;
	var bits = Candid.hash(expr)[0];
	bits &= 0x007f7f7f; // no alpha, and darker than half-grey
	c =  ("00000" + bits.toString(16)).slice(-6);
	return 'color:#' + c;
}

var viewExpr = ({expr, ctx, paren}) => {
	var view = (sym, style) => E('span', {className:'candid-' + expr.kind, style: style}, sym);
	var p = paren ? (x) => E('span', {className:'candid-paren'}, '(', x, ')') : (x) => x;
	if(Candid.closed(expr) < 0)
		ctx = [];
	if(expr.name !== undefined && !expr.expand)
		return view(expr.name, colorExpr(expr._type, ctx));
	switch(expr.kind){
	case 'star': return view('*');
	case 'box': return view('□');
	case 'hole': return view('_');
	case 'hash':
			var n = expr.name;
			return view(n === undefined ? Candid.toId(expr.hash) : n);
	case 'ref':
			var n = ctx[expr.value].argname;
			return view(n === undefined ? expr.value : n, colorExpr(expr._type, ctx));
	case 'rec':
			var n = ctx[expr.value].name;
			return view(n === undefined ? '@'+expr.value : n, colorExpr(expr._type, ctx));
	case 'type': return p(E('span', {className:'candid-type'},
		viewExpr({expr:expr.type, ctx:ctx, paren:true}),
		' | ',
		viewExpr({expr:expr.body, ctx:ctx, paren:false}) ));
	case 'app': return p(E('span', {className:'candid-app'},
		expr.name !== undefined ? E('span',{style:colorExpr(expr._type, ctx)}, expr.name) : '',
		expr.name !== undefined ? ' = ' : '',
		viewExpr({expr:expr.func, ctx:ctx, paren:false}),
		' ',
		viewExpr({expr:expr.arg, ctx:ctx, paren:true}) ));
	case 'pi':
	case 'lam': return p(E('span', {className:'candid-' + expr.kind},
		expr.name !== undefined ? E('span',{style:colorExpr(expr._type, ctx)}, expr.name) : '',
		expr.name !== undefined ? ' = ' : '',
		expr.argname !== undefined ? E('span',{style:colorExpr(expr.type, ctx)}, expr.argname) : '',
		expr.argname !== undefined ? ' : ' : '',
		viewExpr({expr:expr.type, ctx:ctx, paren:true}),
		' → ',
		viewExpr({expr:expr.body, ctx:[expr, ...ctx], paren:false}) ));
	};
};

