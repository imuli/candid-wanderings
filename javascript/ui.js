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
	var p = (level, x) => level <= paren ? E('span', {className:'candid-paren'}, '(', x, ')') : x;
	if(Candid.closed(expr) < 0)
		ctx = [];
	var type;
	try {
		type = Candid.typecheck(expr, ctx);
	} catch (e) {
		type = undefined;
	}
	switch(expr.kind){
	case 'star': return view('★', colorExpr(type, ctx));
	case 'hole': return view('_', colorExpr(type, ctx));
	case 'hash':
			var entry = Candid.fetch(expr.hash);
			var name = entry ? entry.name : expr.name;
			if(name)
				return view(name, colorExpr(type, []));
			if(entry)
				return viewExpr({expr: entry.expr, ctx: [], paren: paren});
			return Candid.toId(expr.hash);
	case 'ref':
			var n = ctx[expr.value] === undefined ? undefined : ctx[expr.value].argname;
			return view(n === undefined ? expr.value : n, colorExpr(type, ctx));
	case 'rec':
			var n = ctx[expr.value] === undefined ? undefined : ctx[expr.value].name;
			return view(n === undefined ? '@'+expr.value : n, colorExpr(type, ctx));
	case 'type': return p(1, E('span', {className:'candid-type'},
		viewExpr({expr:expr.type, ctx:ctx, paren:0}),
		' | ',
		viewExpr({expr:expr.body, ctx:ctx, paren:0}) ));
	case 'app': return p(2, E('span', {className:'candid-app'},
		expr.name ? E('span',{style:colorExpr(type, ctx)}, expr.name) : '',
		expr.name ? ' = ' : '',
		viewExpr({expr:expr.func, ctx:ctx, paren:1}),
		' ',
		viewExpr({expr:expr.arg, ctx:ctx, paren:2}) ));
	case 'pi':
	case 'lam': return p(1, E('span', {className:'candid-' + expr.kind},
		expr.name ? E('span',{style:colorExpr(type, ctx)}, expr.name) : '',
		expr.name ? ' = ' : '',
		expr.argname ? E('span',{style:colorExpr(expr.type, ctx)}, expr.argname) : '',
		expr.argname ? ' : ' : '',
		viewExpr({expr:expr.type, ctx:ctx, paren:1}),
		expr.kind == 'lam' ? ' ⇒ ' : ' → ',
		viewExpr({expr:expr.body, ctx:[expr, ...ctx], paren:0}) ));
	};
};
