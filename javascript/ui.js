"use_strict";
// requires Candid and Inferno to be loaded first

var E = Inferno.createElement;

var colorExpr = (expr) => {
	if(expr === undefined) return undefined;
	var c;
	switch(expr.kind){
	case 'star': c='808080'; break;
	default:
		var bits = Candid.hash(expr)[0];
		bits &= 0x007f7f7f; // no alpha, and darker than half-grey
		c =  ("00000" + bits.toString(16)).slice(-6);
	};
	return 'color:#' + c;
}

var viewExpr = ({expr, ctx, paren}) => {
	var view = (sym, style) => E('span', {className:'candid-' + expr.kind, style: style}, sym);
	var p = paren ? (x) => E('span', {className:'candid-paren'}, '(', x, ')') : (x) => x;
	switch(expr.kind){
	case 'star': return view('*');
	case 'box': return view('□');
	case 'hole': return view('_');
	case 'hash':
			var n = expr.name;
			return view(n === undefined ? toId(expr.hash) : n);
	case 'ref':
			var n = ctx[expr.value].argname;
			return view(n === undefined ? expr.value : n, colorExpr(expr._type));
	case 'rec':
			var n = ctx[expr.value].name;
			return view(n === undefined ? '@'+expr.value : n, colorExpr(expr._type));
	case 'type': return p(E('span', {className:'candid-type'},
		viewExpr({expr:expr.type, ctx:ctx, paren:true}),
		' | ',
		viewExpr({expr:expr.body, ctx:ctx, paren:false}) ));
	case 'app':return p(E('span', {className:'candid-app'},
		viewExpr({expr:expr.func, ctx:ctx, paren:false}),
		' ',
		viewExpr({expr:expr.arg, ctx:ctx, paren:true}) ));
	case 'pi':
	case 'lam':
		if(expr.name !== undefined && !expr.expand)
			return view(expr.name, colorExpr(expr._type));
		return p(E('span', {className:'candid-' + expr.kind},
			expr.name !== undefined ? E('span',{style:colorExpr(expr._type)}, expr.name) : '',
			expr.name !== undefined ? ' = ' : '',
			expr.argname !== undefined ? E('span',{style:colorExpr(expr.type)}, expr.argname) : '',
			expr.argname !== undefined ? ' : ' : '',
			viewExpr({expr:expr.type, ctx:ctx, paren:true}),
			' → ',
			viewExpr({expr:expr.body, ctx:[expr, ...ctx], paren:false}) ));
	};
};

