var state = {
	edits: [],
	focus: 0,
	mode: 'expr',
	scratch: '',
};
var timeout;
var saveState = () => localStorage.setItem('candid-state', JSON.stringify(state));
var loadState = () => JSON.parse(localStorage.getItem('candid-state'));
var redraw = () => {
	clearTimeout(timeout);
	timeout = setTimeout(saveState, 5000);
	return Inferno.render(View.state(state), document.getElementById('app'));
}
Candid.load().then(() => {
	state = loadState();
	if(!state) state = {
		edits: [{
			expr: Candid.Lam(Candid.Star, Candid.Lam(Candid.Ref(0), Candid.Ref(0), 'x'), 't', 'id'),
			focus: [],
		}],
		focus: 0,
	};
	redraw();
});

var keymap = {
	string: {
		keydown: {
			'ArrowUp': State.compose(State.go.up, State.startMode('expr')),
			'Tab': State.compose(State.go.right, State.startMode('expr')),
			'Enter': State.compose(State.go.right, State.startMode('expr')),
			'Backspace': State.str.clear1,
			'ctrl+Backspace': State.str.clear,
		},
		keypress: {
			'default': State.str.append,
		},
	},
	expr: {
		keydown: {
			'ArrowUp': State.go.up,
			'ArrowDown': State.go.down,
			'ArrowLeft': State.go.left,
			'ArrowRight': State.go.right,
			'Home': State.go.home,
			'End': State.go.end,
			'PageUp': State.go.prev,
			'PageDown': State.go.next,
			'shift+Tab': State.go.left,
			'Tab': State.go.right,
			'ctrl+ArrowUp': State.go.prev,
			'ctrl+ArrowDown': State.go.next,
			'ctrl+a': State.go.all,
			'ctrl+k': State.go.up1,
			'Backspace': State.expr.remove,
			'shift+Backspace': State.expr.delete,
			'Enter': State.expr.save,
		},
		keypress: {
			'shift+K': State.go.prev,
			'shift+J': State.go.next,
			'shift+H': State.go.home,
			'shift+L': State.go.end,
			'shift+N': State.compose(State.go.up1, State.go.name),
			'n': State.compose(State.go.up1, State.go.argName),
			'k': State.go.up,
			'j': State.go.down,
			'h': State.go.left,
			'l': State.go.right,
			'shift+V': State.expr.insertExpr,
			'x': State.expr.extractExpr,
			'shift+X': State.expr.extractType,
			'-': State.expr.toggleHash,
			'p': (state) => State.expr.wrap(state, 'pi', 'body'),
			'shift+P': (state) => State.expr.wrap(state, 'pi', 'type'),
			'f': (state) => State.expr.wrap(state, 'lam', 'body'),
			'shift+F': (state) => State.expr.wrap(state, 'lam', 'type'),
			't': (state) => State.expr.wrap(state, 'type', 'body'),
			'shift+T': (state) => State.expr.wrap(state, 'type', 'type'),
			'a': (state) => State.expr.wrap(state, 'app', 'arg'),
			'shift+A': (state) => State.expr.wrap(state, 'app', 'func'),
			's': (state) => State.expr.replace(state, Candid.Star),
			' ': (state) => State.expr.wrap(state, 'app', 'arg', true),
			'shift+_': (state) => State.expr.replace(state, Candid.Hole),
			'm': State.find.match,
			'r': State.find.lookup,
		},
	},
	match: {
		keydown: {
			'Enter':  (state) => State.expr.replace(State.startMode('expr')(state), state.matches[0]),
			'Escape':  State.startMode('expr'),
			'Tab':  State.startMode('expr'),
			'ArrowUp':  State.startMode('expr'),
		},
		// keys 0..9 and a..z replace with match 0..9 and 10..35
		keypress: (() => {
			var keys = {};
			var fn = (state, key) => State.expr.replace(State.startMode('expr')(state), state.matches[parseInt(key, 36)]);
			for(var i = 0; i < 36; i++){
				keys[i.toString(36)] = fn;
			}
			return keys;
		})(),
	},
	lookup: {
		// keys ctrl+0..9 and ctrl+a..z replace with match 0..9 and 10..35
		keydown: (() => {
			var keys = {
				'Enter':  (state) => State.expr.replace(State.startMode('expr')(state), state.matches[0]),
				'Escape':  State.startMode('expr'),
				'Tab':  State.startMode('expr'),
				'ArrowUp':  State.startMode('expr'),
				'Backspace': State.scratch.clear1,
				'ctrl+Backspace': State.scratch.clear,
			};
			var fn = (state, key) => State.expr.replace(State.startMode('expr')(state), state.matches[parseInt(key, 36)]);
			for(var i = 0; i < 36; i++){
				keys['ctrl+'+i.toString(36)] = fn;
			}
			return keys;
		})(),
		keypress: {
			'default': State.scratch.append,
		},
	},
};

var onkey = (event) => {
	var type = state.mode;
	var key = (event.shiftKey?'shift+':'') + (event.ctrlKey?'ctrl+':'') + event.key;
	var act = keymap[type][event.type][key] || keymap[type][event.type].default;
	if(act === undefined) return;
	state = act(state, event.key);
	redraw();
	event.preventDefault();
};

var onclick = (event) => {
	if(!event.target.id) return;
	var [first, ...path] = event.target.id.split('!');
	switch(first){
		case 'match':
			var match = state.matches[path|0];
			if(match === undefined) return;
			state = State.expr.replace(state, match);
			break;
		default:
			first |= 0;
			if(state.edits[first] === undefined) return;
			state = State.update(State.update(state, ['focus'], first), ['edits', first, 'focus'], path);
			break;
	}
	redraw();
	event.preventDefault();
	console.log(event);
};

window.addEventListener('keypress', onkey, {capture: true});
window.addEventListener('keydown', onkey, {capture: true});
window.addEventListener('click', onclick, {capture: true});
window.addEventListener('beforeunload', saveState);
