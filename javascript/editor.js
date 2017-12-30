var state = {
	exprs: [],
	focus: ['0'],
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
		exprs: [Candid.Lam(Candid.Star, Candid.Lam(Candid.Ref(0), Candid.Ref(0), 'x'), 't', 'id')],
		focus: ['0'],
	};
	redraw();
});

var keymap = {
	string: {
		keydown: {
			'ArrowUp': State.go.up,
			'shift+Tab': State.go.left,
			'Tab': State.go.right,
			'Enter': State.go.right,
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
			'PageUp': State.go.prev,
			'PageDown': State.go.next,
			'shift+Tab': State.go.left,
			'Tab': State.go.right,
			'ctrl+ArrowUp': State.go.prev,
			'ctrl+ArrowDown': State.go.next,
			'ctrl+a': State.go.all,
			'ctrl+k': State.go.up1,
			'Backspace': State.expr.remove,
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
			'p': (state) => State.expr.wrap(state, 'pi', 'type'),
			'shift+P': (state) => State.expr.wrap(state, 'pi', 'body'),
			'f': (state) => State.expr.wrap(state, 'lam', 'type'),
			'shift+F': (state) => State.expr.wrap(state, 'lam', 'body'),
			't': (state) => State.expr.wrap(state, 'type', 'type'),
			'shift+T': (state) => State.expr.wrap(state, 'type', 'body'),
			'a': (state) => State.expr.wrap(state, 'app', 'func'),
			'shift+A': (state) => State.expr.wrap(state, 'app', 'arg'),
			's': (state) => State.expr.replace(state, Candid.Star),
			'shift+_': (state) => State.expr.replace(state, Candid.Hole),
		},
	}
};

var onkey = (event) => {
	// are we editing a string or an expression?
	var type = {
		name: 'string',
		argname: 'string',
	}[State.lastStep(state)] || 'expr';

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
	first |= 0;
	if(state.edits[first] === undefined) return;
	state = State.update(State.update(state, ['focus'], first), ['edits', first, 'focus'], path);
	redraw();
	event.preventDefault();
	console.log(event);
};

window.addEventListener('keypress', onkey, {capture: true});
window.addEventListener('keydown', onkey, {capture: true});
window.addEventListener('click', onclick, {capture: true});
window.addEventListener('beforeunload', saveState);
