var View = (() => {
	var E = Inferno.createElement;

	var muddle = (expr, depth) => {
		if(Candid.closed(expr) < 0) return expr;
		switch(expr.kind){
			case 'ref': return Candid.Ref(depth - expr.value);
			case 'rec': return Candid.Rec(depth - expr.value);
			case 'type':
				return Candid.Type(muddle(expr.type, depth), muddle(expr.body, depth), expr.name);
			case 'app':
				return Candid.App(muddle(expr.func, depth), muddle(expr.arg, depth), expr.name);
			case 'pi':
				return Candid.Pi(muddle(expr.type, depth), muddle(expr.body, depth+1), expr.argname, expr.name);
			case 'lam':
				return Candid.Lam(muddle(expr.type, depth), muddle(expr.body, depth+1), expr.argname, expr.name);
		}
		return expr;
	};

	var colorExpr = (expr, ctx) => {
		var hash = Candid.hash(muddle(expr, ctx.length));
		var hex = blake2s1.toHex(hash.slice(0,1));
		//return {style: 'color:#' + hex.slice(0,6)};
		return {style: 'color:#' + ('000000' + (hash[0] & 0x007f7f7f7f).toString(16)).slice(-6)};
	};

	var parens = {
		star: 1.0,
		hole: 1.0,
		ref: 1.0,
		rec: 1.0,
		hash: 1.0,
		app: 0.5,
		pi: 0.1,
		type: 0.1,
		lam: 0.1,
	};

	var viewExpr = (expr, ctx, focus, path, paren) => {
		if(ctx === undefined) ctx = [];
		if(path === undefined) path = [];
		if(focus === undefined) focus = [null];
		if(paren === undefined) paren = 0;

		if(Candid.closed(expr) < 0) ctx = [];

		// does this expression have the focus?
		var f = (...here) => focus.length === here.length && focus.every((v,i) => v === here[i]);
		// does this expression need parentheses?
		var p = parens[expr.kind] <= paren;
		// helper function to show this expression
		var view = (params, ...body) => E('span',
			Object.assign({
				id: path.join('!'),
				className: 'candid-' + expr.kind + (f(...path) ? ' candid-focus' : '')
			}, params),
			p?'(':'', ...body, p?')':''
		);
		// helper function to show names
		var name = (params, which) => expr[which] || f(...path, which)
			?	[ E('span', Object.assign({
				id: path.join('!') + '!' + which,
				className: 'candid-' + which + (f(...path, which) ? ' candid-focus' : '')
			}, params), expr[which]), { name: ' = ', argname: ' : ' }[which]
			] : [];

		// determine which color to make this expression (or it's name)
		var color;
		try {
			color = colorExpr(Candid.typecheck(expr, ctx), ctx);
		} catch (e) {
			console.warn(e);
			color = {style: 'color:black'};
		}

		switch(expr.kind){
			case 'star': return view(color, '★');
			case 'hole': return view(color, '_');
			case 'ref': return view(color, (ctx[expr.value] || {}).argname || ('!' + expr.value));
			case 'rec': return view(color, (ctx[expr.value] || {}).name || ('@' + expr.value));
			case 'hash':
				var entry = Candid.fetch(expr.hash);
				switch(true){
					case !entry:
						return view(color, blake2s1.toHex(expr.hash));
					case !entry.name:
						return viewExpr(entry. expr, [], focus, path, paren);
					default:
						return view(color, entry.name);
				}
			case 'app':
				return view({},
					...(name(color, 'name')),
					viewExpr(expr.func, ctx, focus, [...path, 'func'], 0.1),
					' ',
					viewExpr(expr.arg, ctx, focus, [...path, 'arg'], 0.5),
				);
			case 'type':
				return view({},
					...(name(color, 'name')),
					viewExpr(expr.type, ctx, focus, [...path, 'type'], 0.1),
					E('br', {}),
					viewExpr(expr.body, ctx, focus, [...path, 'body'], 0.0),
				);
			case 'pi':
			case 'lam':
				return view({},
					...(name(color, 'name')),
					...(name(colorExpr(expr.type, ctx), 'argname')),
					viewExpr(expr.type, ctx, focus, [...path, 'type'], 0.1),
					{lam:' ⇒ ', pi:' → '}[expr.kind],
					viewExpr(expr.body, [expr, ...ctx], focus, [...path, 'body'], 0.0),
				);
		};
	};

	var viewTypeAt = (path, expr) => {
		try {
			var {type, ctx} = Candid.typeAt(path, expr);
			return viewExpr(Candid.enhash(type), ctx, undefined, ["typeat"]);
		} catch(e) {
			console.warn(e, path, expr);
			return e.toString();
		}
	};

	var viewType = (expr, ctx, path) => {
		try {
			var type = Candid.typecheck(expr, ctx);
			return viewExpr(Candid.enhash(type), ctx, undefined, ["type"]);
		} catch(e) {
			console.log(expr, ctx);
			console.warn(e);
			return e.toString();
		}
	};

	var viewState = (state) => {
		// where is our current focus
		var step = state.focus;
		var path = state.edits[step].focus;
		// don't try to typecheck a name/argname
		if(path[path.length-1] in {argname:1, name:1})
			path = path.slice(0,-1);
		// we'll need the expression and context of our focus
		// below to show the desired type and the actual type
		var {expr, ctx} = Candid.lookup(state.edits[step].expr, path, []);
		// render all of the current expressions
		var edits = state.edits.map((edit, i) => E('div',
			{className: 'candid-expr' + (state.focus == i ? ' candid-focus' : '')},
			viewExpr(edit.expr, [], [i, ...edit.focus], [i])
		));
		var {type} = Candid.typeAt(path, state.edits[step].expr);
		state.matches = Candid.search(type, ctx);
		var matches = state.matches.map((option, i) => E('li',
			{className: 'candid-expr'},
			viewExpr(option, ctx),
			':',
			viewType(option, ctx),
		));
		return E('div', {}, 
			E('div', { className: 'candid-typeat' },
				"Need Type: ",
				viewTypeAt(path, state.edits[step].expr)
			),
			E('div', { className: 'candid-type' },
				"Have Type: ",
				viewType(expr, ctx)
			),
			...edits,
			E('ul', { className: 'candid-matches' },
				...matches
			)
		);
	};
	return {
		state: viewState,
	};
})();

