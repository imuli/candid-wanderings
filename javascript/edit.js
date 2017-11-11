var E = Inferno.createElement;
var state =
	{ focus: ['edit'],
		expr: Candid.Hole(''),
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
		},
		string === '' ? '\xa0' : string,
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
			case event.type == 'keydown' && event.key == 'Enter':
				state.expr = Candid.store(state.expr, true);
				state.focus = ['edit'];
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
						console.log(_path, expr);
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
			case event.key == 'r': // reference
				state.expr = Candid.update(e, _path, Candid.Ref());
				break;
			case event.key == 'R': // Recursion
				state.expr = Candid.update(e, _path, Candid.Rec());
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
			// for filling in references and recurs
			case expr.kind == 'ref' && !isNaN(parseInt(event.key)):
				state.expr = Candid.update(e, _path, Candid.Ref(event.key|0));
				break;
			case expr.kind == 'rec' && !isNaN(parseInt(event.key)):
				state.expr = Candid.update(e, _path, Candid.Rec(event.key|0));
				break;
			default:
				console.log(event.key);
				return;
		}
		redraw();
		event.preventDefault();
		event.stopPropagation();
	};

	var focus = (event) => {
		state.focus = path;
		redraw();
		event.preventDefault();
		event.stopPropagation();
	};

	var p = paren ? (x) => E('span', {className:'candid-paren'}, '(', x, ')') : (x) => x;

	var ed = (props, ...children) => E('span',
		Object.assign({ className: 'candid-' + expr.kind,
			onKeyPress: key,
			onKeyDown: key,
			onFocus: focus,
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
			expr.value === undefined ? '?' : ctx[expr.value] && ctx[expr.value].argname ? ctx[expr.value].argname : expr.value.toString(),
		);
		case 'rec': return ed({},
			expr.value === undefined ? '?' : ctx[expr.value] && ctx[expr.value].name ? ctx[expr.value].name : expr.value.toString(),
		);
		case 'hash':
			var entry = Candid.fetch(expr.hash);
			return ed({},
				entry ? entry.name : expr.name === undefined ? Candid.toId(expr.hash) : expr.name,
			);
		case 'app': return p(ed({},
			exprEdit([...path, 'func'], expr.func, ctx, false),
			' ',
			exprEdit([...path, 'arg'], expr.arg, ctx, true),
		));
		case 'pi':
		case 'lam': return p(ed({},
			stringEdit([...path, 'name'], expr.name),
			expr.name ? ' = ' : '',
			stringEdit([...path, 'argname'], expr.argname),
			expr.argname ? ' : ' : '',
			exprEdit([...path, 'type'], expr.type, ctx, true),
			expr.kind == 'lam' ? ' → ' : ' ⇒ ',
			exprEdit([...path, 'body'], expr.body, [expr, ...ctx], false),
		));
	}

};

var listEntry = (expr) => {
	click = (event) => {
		if(state.focus.length == 1){
			state.expr = Candid.unwrap(expr);
		} else {
			state.expr = Candid.update(state.expr, state.focus.slice(1), expr);
		}
		redraw();
		event.preventDefault();
		event.stopPropagation();
	};
	return E('li', { onClick: click }, viewExpr({expr:expr, ctx:[]}));
};

var storeList = () => {
	var list = [];
	for(var h in Candid._store){
		var s = Candid._store[h];
		var e = Candid.Hash(s.hash, s.name);
		e._type = s.type;
		list.push(listEntry(e));
	};
	return E('ul', { className: 'candid-list' }, ...list);
};

var viewType = (expr) => {
	try  {
		var type = Candid.typecheck(expr);
		type = Candid.store(type); // for the hash reduction
		return E('div', { className: 'candid-typecheck' }, viewExpr({expr:type, ctx:[]}));
	} catch (e) {
		return E('div',
			{ className: 'candid-typeerror' },
			E('h2', {}, e.kind),
			e.exp ? viewExpr({expr:e.exp, ctx:e.ctx}) : undefined,
			e.ft ? viewExpr({expr:e.ft, ctx:e.ctx}) : undefined,
			e.et ? viewExpr({expr:e.et, ctx:e.ctx}) : undefined,
			e.at ? viewExpr({expr:e.at, ctx:e.ctx}) : undefined,
		);
	}
};

var view = () => E('div', null,
	exprEdit(['edit'], state.expr),
	viewType(state.expr),
	storeList(),
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

