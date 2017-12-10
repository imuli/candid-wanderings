var E = Inferno.createElement;
var state =
	{ focus: ['edit'],
		expr: Candid.Hole(''),
	};

var focus = (path) => (event) => {
	state.focus = path;
	redraw();
	event.preventDefault();
	event.stopPropagation();
};

var toNext = {
	name: ['argname', 'func'],
	argname: ['type'],
	type: ['body'],
	func: ['arg'],
};

var toPrev = {
	argname: ['name'],
	type: ['argname'],
	body: ['type'],
	func: ['name'],
	arg: ['func'],
};

var shiftFocus = (map, path) => {
	if(path.length == 1) return;
	var cur = path[path.length-1];
	var par = path.slice(0,-1);
	var expr = Candid.byPath(state.expr, par.slice(1));
	if(cur in map){
		for(var next of map[cur]){
			if(next in expr){
				state.focus = [...par, next];
				return;
			}
		}
	}
	return shiftFocus(map, par);
}

var stringEdit = (path, string) => {
	string = string === undefined ? '' : string;
	var key = (event) => {
		state.focus = path;
		var _path = path.slice(1);
		switch(true){
			case event.key == 'Backspace' && event.ctrlKey:
				state.expr = Candid.update(state.expr, _path, '');
				break;
			case event.key == 'Backspace':
				state.expr = Candid.update(state.expr, _path, string.slice(0,-1));
				break;
			case event.key == 'Escape':
			case event.key == 'ArrowUp':
				state.focus = path.slice(0,-1);
				break;
			case event.type == 'keydown':
				return;
			case event.key.length == '1':
				state.expr = Candid.update(state.expr, _path, string + event.key);
				break;
			default:
				return;
		}
		redraw();
		event.preventDefault();
		event.stopPropagation();
	};
	return E('span',
		{ id: path.join('!'),
			tabindex: 0,
			onKeyPress: key,
			onKeyDown: key,
			className: 'candid-' + path[path.length-1],
		},
		string,
	);
};

// Levenshtein distance, where capitalization costs 0.5, between strings a and b.
var editDist = (a,b) => {
	if(a.length < b.length) [a,b] = [b,a];
	var prev = []; // prev = [0..b.length]
	for(var i = 0; i < b.length+1; i++) prev.push(i);
	for(var i = 0; i < a.length; i++){
		var cur = [i+1];
		for(var j = 0; j < b.length; j++){
			var cost = a.charAt(i) === b.charAt(j) ? 0 :
				a.charAt(i).toLowerCase() === b.charAt(j).toLowerCase() ? 0.5 : 1;
			cur.push(Math.min(prev[j+1]+1, cur[j]+1, prev[j] + cost));
		}
		prev = cur;
	}
	return cur[b.length];
}

var hashEdit = (path, string) => {
	string = string === undefined ? '' : string;
	var key = (event) => {
		state.focus = path;
		var _path = path.slice(1);
		switch(true){
			case event.key == 'Backspace' && event.ctrlKey:
				state.expr = Candid.update(state.expr, _path, '');
				break;
			case event.key == 'Backspace':
				state.expr = Candid.update(state.expr, _path, string.slice(0,-1));
				break;
			case event.key == 'Escape':
			case event.key == 'ArrowUp':
				state.focus = path.slice(0,-1);
				break;
			case event.key == 'Enter':
				// update the parent with the contents of the first match
				var target = event.target.children[0].children[0];
				if(target) target.click();
				break;
			case event.ctrlKey && !isNaN(parseInt(event.key)):
				var target = event.target.children[0].children[parseInt(event.key)];
				if(target) target.click();
				break;
			case event.type == 'keydown':
				return;
			case event.key.length == '1':
				state.expr = Candid.update(state.expr, _path, string + event.key);
				break;
			default:
				return;
		}
		redraw();
		event.preventDefault();
		event.stopPropagation();
	};

	var search;
	if(state.focus.join('!') == path.join('!')){
		var list = [];
		var typePat = Candid.typeAt(path.slice(1, -1), state.expr);
		var name = string.toLowerCase();
		for(var k in Candid._store){
			var entry = Candid._store[k];
			if(entry && entry.name && (!entry.type || Candid.typeMatch(typePat, entry.type))){
				var e = Candid.Hash(entry.hash, entry.name);
				e._dist = entry.name.toLowerCase().indexOf(name);
				if(e._dist < 0) e._dist = name.length;
				e._dist += editDist(string, entry.name)/entry.name.length;
				list.push(e);
			}
		}
		list = list.sort((a,b) =>
			(a._dist - b._dist) ||
			(a.name < b.name ? -1 : 1)
		).slice(0,10);
		search = E('table', { className: 'candid-matches' }, ...list.map((e)=>listEntry(e, [])));
	}
	return E('span',
		{ id: path.join('!'),
			tabindex: 0,
			onKeyPress: key,
			onKeyDown: key,
			onFocus: focus(path),
		},
		string === '' ? '﹟' : string,
		search,
	);
};

var reEdit = (path, refrec, ctx) => {
	var key = (event) => {
		state.focus = path;
		var _path = path.slice(1);
		switch(true){
			case event.key == 'Backspace':
			case event.key == 'Escape':
			case event.key == 'ArrowUp':
				state.focus = path.slice(0,-1);
				state.expr = Candid.update(state.expr, _path.slice(0,-1), Candid.Hole(''));
				break;
			case event.key == 'Enter':
				// select the first non-dim match TODO hackish
				for(var target of event.target.children[0].children){
					if(target.style.filter == ""){
						target.click();
						break;
					}
				}
				break;
			case event.type == 'keydown':
				return;
			case event.key.length == '1' && !isNaN(parseInt(event.key,36)):
				var target = event.target.children[0].children[parseInt(event.key, 36)];
				if(target) target.click();
				break;
			default:
				return;
		}
		redraw();
		event.preventDefault();
		event.stopPropagation();
	};

	var search;
	if(state.focus.join('!') == path.join('!')){
		var list = [];
		var typePat = Candid.typeAt(path.slice(1, -1), state.expr);
		for(var k in ctx){
			var expr = refrec(k|0);
			var dim;
			try {
				var type = Candid.typecheck(expr, ctx);
				dim = !Candid.typeMatch(typePat, type);
			} catch (e) {
				dim = true;
			}
			list.push(listEntry(refrec(k|0), ctx, dim));
		}
		search = E('table', { className: 'candid-matches' }, ...list);
	}
	return E('span',
		{ id: path.join('!'),
			tabindex: 0,
			onKeyPress: key,
			onKeyDown: key,
			onFocus: focus(path),
		},
		'?',
		search,
	);
};

var exprEdit = (path, expr, ctx, paren) => {

	var key = (event) => {
		// don't touch strokes with modifiers
		if(event.ctrlKey || event.altKey || event.metaKey) return;
		var e = state.expr;
		var _path = path.slice(1);
		state.focus = path;
		switch(true){
			case event.key == 'ArrowUp':
				if(path.length > 1)
					state.focus = path.slice(0,-1);
				break;
			case event.key == 'ArrowDown':
				var sub = {pi: 'body', lam: 'body', type: 'body', app: 'arg'}[expr.kind];
				if(sub !== undefined)
					state.focus = [...path, sub];
				break;
			case event.type == 'keydown' && event.key == 'Enter':
				if(event.shiftKey){
					Candid.store(Candid.typecheck(state.expr), true);
				} else {
					state.expr = Candid.store(state.expr, true);
					state.focus = ['edit'];
				}
				Candid.save();
				break;
			case event.type == 'keydown' && event.key == 'Backspace':
				if(_path.length == 0){
					state.expr = Candid.Hole('');
					break;
				}
				var mpath = _path[_path.length-1];
				var ppath = _path.slice(0,-1);
				var pexp = Candid.byPath(e, ppath);
				switch(pexp.kind){
					case 'lam':
					case 'pi':
						expr = mpath == 'type' ? Candid.shift(-1, pexp.body) : pexp.type;
						_path = ppath;
						break;
					case 'type':
						expr = mpath == 'type' ? pexp.body : pexp.type;
						_path = ppath;
						break;
					case 'app':
						expr = mpath == 'func' ? pexp.arg : pexp.func;
						_path = ppath;
						break;
				}
				state.expr = Candid.update(e, _path, expr);
				state.focus = ['edit', ..._path];
				break;
			case event.type == 'keydown' && event.shiftKey && event.key == 'Delete':
				state.expr = Candid.update(e, _path, Candid.unwrap(expr));
				Candid.remove(expr);
				Candid.save();
				break;
			case event.type == 'keydown' && event.key == 'Tab':
			case event.type == 'keydown':
				return;
			// nullary expressions
			case event.key == '*':
			case event.key == 's': // star
				state.expr = Candid.update(e, _path, Candid.Star);
				shiftFocus(toNext, path);
				break;
			case event.key == '_': // _ from haskell, etc
				state.expr = Candid.update(e, _path, Candid.Hole(''));
				break;
			// unary expressions
			case event.key == 'e': // hash lookup
				state.expr = Candid.update(e, _path, Candid.Hash(Candid.hash0, ''));
				state.focus = [...path, 'name'];
				break;
			case event.key == 'r': // reference
				state.expr = Candid.update(e, _path, Candid.Ref());
				state.focus = [...path, 'value'];
				break;
			case event.key == 'R': // Recursion
				state.expr = Candid.update(e, _path, Candid.Rec());
				state.focus = [...path, 'value'];
				break;
			// binary expressions
			case event.key == 'f': // function
			case event.key == '\\': // \ from haskell, etc
			case event.key == 'λ': // lambda
				state.expr = Candid.update(e, _path, Candid.Lam(expr, Candid.Hole('')));
				state.focus = [...path, path.length == 1 ? 'name' : 'argname'];
				break;
			case event.key == 'F': // Function
			case event.key == 'Λ': // Lambda
				state.expr = Candid.update(e, _path, Candid.Lam(Candid.Hole(''), Candid.shift(1, expr)));
				state.focus = [...path, path.length == 1 ? 'name' : 'argname'];
				break;
			case event.key == 'p': // pi
			case event.key == 'π': // pi
				state.expr = Candid.update(e, _path, Candid.Pi(expr, Candid.Hole('')));
				state.focus = [...path, path.length == 1 ? 'name' : 'argname'];
				break;
			case event.key == 'P': // Pi
			case event.key == 'Π': // Pi
				state.expr = Candid.update(e, _path, Candid.Pi(Candid.Hole(''), Candid.shift(1, expr)));
				state.focus = [...path, path.length == 1 ? 'name' : 'argname'];
				break;
			case event.key == 't': // type
				state.expr = Candid.update(e, _path, Candid.Type(expr, Candid.Hole('')));
				state.focus = [...path, expr.kind == 'hole' ? 'type' : 'body'];
				break;
			case event.key == 'T': // Type
				state.expr = Candid.update(e, _path, Candid.Type(Candid.Hole(''), expr));
				state.focus = [...path, 'type'];
				break;
			case event.key == 'a': // apply
			case event.key == '(':
				state.expr = Candid.update(e, _path, Candid.App(expr, Candid.Hole('')));
				state.focus = [...path, expr.kind == 'hole' ? 'func' : 'arg'];
				break;
			case event.key == 'A': // Apply
				state.expr = Candid.update(e, _path, Candid.App(Candid.Hole(''), expr));
				state.focus = [...path, 'func'];
				break;
			// convenience
			case event.key == '-': // unwrap and enhash expressions
				if(expr.kind == 'hash'){
					state.expr = Candid.update(e, _path, Candid.unwrap(expr));
				} else {
					state.expr = Candid.update(e, _path, Candid.enhash(expr));
				}
				break;
			case event.key == '=': // reduction
				state.expr = Candid.update(e, _path, Candid.enhash(Candid.reduce(Candid.unhash(expr), false)));
				break;
			case event.key == '+': // recursive reduction
				state.expr = Candid.update(e, _path, Candid.enhash(Candid.reduce(Candid.unhash(expr), true)));
				break;
			case event.key == '~': // convert Pi to Lam and back
				var f = {pi: Candid.Lam, lam: Candid.Pi}[expr.kind];
				if(f){
					state.expr = Candid.update(e, _path, f(expr.type, expr.body, expr.argname, expr.name));
				}
				break;
			case event.key == ')': // encapsulate parent Apply's parent Apply in new Apply
				if(_path[_path.length-1] == 'arg'){
					_path = _path.slice(0,-1);
					expr = Candid.byPath(e, _path);
				} // fall through
			case event.key == ' ': // encapsulate parent Apply in a new Apply
				if(_path[_path.length-1] == 'arg'){
					_path = _path.slice(0,-1);
					expr = Candid.byPath(e, _path);
				} else if(_path[_path.length-1] == 'func') {
					state.focus = ['edit', ..._path.slice(0,-1), 'arg'];
					break;
				}
				state.expr = Candid.update(e, _path, Candid.App(expr, Candid.Hole('')));
				state.focus = ['edit', ..._path, 'arg']
				break;
			// expression naming
			case expr.kind in {'pi':1,'lam':1,'app':1,'type':1} && event.key == 'n':
				state.focus = [...path, 'name'];
				break;
			default:
				return;
		}
		redraw();
		event.preventDefault();
		event.stopPropagation();
	};

	var p = (level, x) => level <= paren ? E('span', {className:'candid-paren'}, '(', x, ')') : x;

	var ed = (props, ...children) => E('span',
		Object.assign({ className: 'candid-' + expr.kind,
			onKeyPress: key,
			onKeyDown: key,
			onFocus: focus(path),
			tabindex: 0,
			id: id,
		}, props),
		...children
	);

	if(ctx === undefined) ctx = [];
	var id = path.join('!');
	switch(expr.kind){
		case 'hole': return ed({}, '\xa0');
		case 'star': return ed({}, '★');
		case 'ref': return ed({},
			expr.value === undefined ? reEdit([...path, 'value'], Candid.Ref, ctx) : ctx[expr.value] && ctx[expr.value].argname ? ctx[expr.value].argname : expr.value.toString(),
		);
		case 'rec': return ed({},
			expr.value === undefined ? reEdit([...path, 'value'], Candid.Rec, ctx) : ctx[expr.value] && ctx[expr.value].name ? ctx[expr.value].name : expr.value.toString(),
		);
		case 'hash':
			var entry = Candid.fetch(expr.hash);
			var name = entry ? entry.name : expr.name;
			switch(true){
				case !entry:
					return ed({}, hashEdit([...path, 'name'], name));
				case !!name:
					return ed({}, name);
				default:
					state.expr = Candid.update(state.expr, path.slice(1), entry.expr);
					return exprEdit(path, entry.expr, ctx, paren);
			}
		case 'app': return p(2, ed({},
			expr.name || state.focus.join('!') == id+'!name' ?
				stringEdit([...path, 'name'], expr.name) : null,
			expr.name ? E('span', {className:'candid-equals'}, ' = ') : '',
			exprEdit([...path, 'func'], expr.func, ctx, 1),
			' ',
			exprEdit([...path, 'arg'], expr.arg, ctx, 2),
		));
		case 'pi':
		case 'lam': return p(1, ed({},
			expr.name || state.focus.join('!') == id+'!name' ?
				stringEdit([...path, 'name'], expr.name) : null,
			expr.name ? E('span', {className:'candid-equals'}, ' = ') : '',
			stringEdit([...path, 'argname'], expr.argname),
			expr.argname ? ' : ' : '',
			exprEdit([...path, 'type'], expr.type, ctx, 1),
			E('span', {className:'candid-arrow'}, expr.kind == 'lam' ? ' → ' : ' ⇒ '),
			exprEdit([...path, 'body'], expr.body, [expr, ...ctx], 0),
		));
		case 'type': return p(1, ed({},
			expr.name || state.focus.join('!') == id+'!name' ?
				stringEdit([...path, 'name'], expr.name) : null,
			expr.name ? E('span', {className:'candid-equals'}, ' = ') : '',
			exprEdit([...path, 'type'], expr.type, ctx, 0),
			E('br',{}),
			exprEdit([...path, 'body'], expr.body, ctx, 0),
		));
	}

};

var listEntry = (expr, ctx, dim) => {
	click = (event) => {
		switch(state.focus[state.focus.length-1]){
			case 'state':
				state.expr = Candid.unwrap(expr);
			case 'argname':
			case 'name':
			case 'value':
				state.expr = Candid.update(state.expr, state.focus.slice(1,-1), expr);
				state.focus = state.focus.slice(0,-1);
				break;
			default:
				state.expr = Candid.update(state.expr, state.focus.slice(1), expr);
				break;
		}
		shiftFocus(toNext, state.focus);
		redraw();
		event.preventDefault();
		event.stopPropagation();
	};
	var type;
	try {
		type = viewExpr({expr:Candid.enhash(Candid.typecheck(expr, ctx)), ctx:ctx});
	} catch (e) {
		type = e.kind;
	}
	return E('tr', { onClick: click, style: dim ? "filter:opacity(50%);" : "" },
		E('td', {}, viewExpr({expr:expr, ctx:ctx})),
		E('td', {}, ':'),
		E('td', {}, type),
	);
};

var storeList = () => {
	var list = [];
	for(var h in Candid._store){
		var s = Candid._store[h];
		if(!s) continue;
		var e = Candid.Hash(s.hash, s.name);
		list.push(listEntry(e, []));
	};
	return E('table', { className: 'candid-list' }, ...list);
};

var viewType = (expr) => {
	try  {
		var type = Candid.typecheck(expr);
		type = Candid.enhash(type);
		try { Candid.typecheck(type); } catch (e) {};
		return E('div', { className: 'candid-typecheck' }, viewExpr({expr:type, ctx:[]}));
	} catch (e) {
		console.warn(e);
		return E('div',
			{ className: 'candid-typeerror' },
			E('h3', {}, e.kind),
			e.exp ? viewExpr({expr:e.exp, ctx:e.ctx}) : undefined,
			e.ft ? viewExpr({expr:e.ft, ctx:e.ctx}) : undefined,
			e.et ? viewExpr({expr:e.et, ctx:e.ctx}) : undefined,
			e.at ? viewExpr({expr:e.at, ctx:e.ctx}) : undefined,
		);
	}
};

var saveLink = (data, name) => {
	var click = (event) => {
		var str = JSON.stringify(data, null, '\t');
		var blob = new Blob([str], { type:'application/json' });
		event.target.href = URL.createObjectURL(blob);
	};
	return E('a', {
		href: 'placeholder:',
		onClick: click,
		download: name + '.json',
	}, "Save " + name);
};

var view = () => E('div', {className:'edit'},
	saveLink(Candid._store, "store"),
	saveLink(state.expr, "expression"),
	viewType(state.expr),
	E('div', {}, exprEdit(['edit'], state.expr)),
);
var redraw = () => {
	Inferno.render(view(), document.getElementById('app'));
	try {
		document.getElementById(state.focus.join('!')).focus();
	} catch (e) {
		console.warn(e, state.focus);
	}
}

Candid.load().then(() => redraw());

