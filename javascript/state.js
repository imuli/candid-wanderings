/*
var Editor = {
	expr: expr;
	focus: [step];
}
var State = {
	edits: [Editor];
  focus: number;
};
*/
var State = (() => {
  var getArray = (arr, [step, ...rest], getElem) => {
    if(step === undefined){
      return arr;
    }
		return getElem(arr[step], rest);
  };

  var updateArray = (state, [step, ...rest], repl, updateElem) => {
    if(step === undefined){
      if(repl.constructor == Array) return repl;
      return state;
    }
    var n = state.slice();
    n[step] = updateElem(n[step], rest, repl);
    return n;
  };

	var getEditor = (editor, [step, ...rest]) => {
		switch(step){
      case 'focus':
        return editor.focus;
      case 'expr':
				return Candid.lookup(editor.expr, rest);
			case undefined:
				return editor;
			default:
        throw "Bad Path: " + step;
		};
	};

	var updateEditor = (editor, [step, ...rest], repl) => {
		switch(step){
      case 'focus':
        return Object.assign({}, editor, {
					focus: updateArray(editor.exprs, rest, repl, (orig, path, repl) => repl),
				});
      case 'expr':
        return Object.assign({}, editor, {
          expr: Candid.update(editor.expr, rest, repl),
				});
			case undefined:
				return Object.assign({}, editor, repl);
			default:
        throw "Bad Path: " + step;
		};
	};

  var get = (state, [step, ...rest]) => {
    switch(step){
      case 'focus':
        return state.focus
      case 'edits':
				return getArray(state.edits, rest, getEditor);
      case undefined:
        return state;
      default:
        throw "Bad Path: " + step;
    };
  };

  var update = (state, [step, ...rest], repl) => {
    switch(step){
      case 'focus':
        return Object.assign({}, state, {focus: repl});
      case 'edits':
        return Object.assign({}, state, {
          edits: updateArray(state.edits, rest, repl, updateEditor),
				});
      case undefined:
				return Object.assign({}, state, repl);
      default:
        throw "Bad Path: " + step;
    };
  };

	var addEdit = (state, expr, focus) => {
		if(focus === undefined) focus = [];
		if(expr === undefined) expr = Candid.Hole;
		return update(state, ['edits'], [...state.edits, {expr: expr, focus: focus}]);
	};

	var edit = (state, path, repl) => update(state, ['edits', state.focus, ...path], repl);
	var getEdit = (state, path) => get(state, ['edits', state.focus, ...path]);
	var addFocus = (state, ...more) => edit(state, ['focus'], [...getEdit(state, ['focus']), ...more]);
	var getFocus = (state) => state.edits[state.focus].focus;
	var getFocusExpr = (state) => get(state, ['edits', state.focus, 'expr', ...state.edits[state.focus].focus]);
	var lastStep = (state, n) => {
		if(n === undefined) n = 1;
		var focus = state.edits[state.focus].focus;
		return focus[focus.length-n];
	};

  var lookup = (state, path) => getEdit(state, ['expr', ...path]);

  var compose = (first, then) => (state) => then(first(state));

  var goAllTheWay = (func, state) => {
    while(true){
      var newstate = func(state);
      if(newstate == state) return newstate;
      state = newstate;
    }
  };

	var goUp1 = (state) => {
		var focus = getEdit(state, ['focus']);
		if(focus.length == 0) return state;
		return edit(state, ['focus'], focus.slice(0,-1));
	};

  var goPrev = (state) => state.focus === 0 ? state
		: update(state, ['focus'], state.focus - 1);

  var goNext = (state) => state.focus === state.edits.length - 1 ? state
      : update(state, ['focus'], state.focus + 1);

  var goName = (state) => {
    var {expr} = getFocusExpr(state);
    switch(true){
      case expr && expr.kind == 'pi':
      case expr && expr.kind == 'lam':
      case expr && expr.kind == 'type':
      case expr && expr.kind == 'app':
      case expr && expr.kind == 'hash':
        return addFocus(state, 'name');
      default:
        return state;
    }
  };

  var goArgName = (state) => {
    var {expr} = getFocusExpr(state);
    switch(true){
      case expr && expr.kind == 'pi':
      case expr && expr.kind == 'lam':
        return addFocus(state, 'argname');
      default:
        return state;
    }
  };

  var goDownLeft = (state) => {
    var {expr} = getFocusExpr(state);
    switch(true){
      case expr && expr.kind == 'pi':
      case expr && expr.kind == 'lam':
      case expr && expr.kind == 'type':
        return addFocus(state, 'type');
      case expr && expr.kind == 'app':
        return addFocus(state, 'func');
      default:
        return state;
    }
  };

  var goDownRight = (state) => {
    var {expr} = getFocusExpr(state);
    switch(true){
      case expr && expr.kind == 'pi':
      case expr && expr.kind == 'lam':
      case expr && expr.kind == 'type':
        return addFocus(state, 'body');
      case expr && expr.kind == 'app':
        return addFocus(state, 'arg');
      default:
        return state;
    }
  };

  var goLeft = (state) => {
    switch(lastStep(state)){
      case 'argname':
        return addFocus(goUp1(state), 'name');
      case 'type':
        if(lastStep(state, 2) == 'body')
          return goLeft(goUp1(state));
        return state;
      case 'body':
        return goDownLeft(goUp1(state));
      case 'arg':
        return goDownRight(goDownLeft(goUp1(state)));
      case 'func':
      default:
        return state;
    }
  };

  var goRight = (state) => {
    switch(lastStep(state)){
      case 'name':
        var {expr} = getFocusExpr(goUp1(state));
        var into = {
          pi: 'argname',
          lam: 'argname',
          type: 'type',
          app: 'func',
        }[expr.kind];
        return addFocus(goUp1(state), into);
      case 'type':
        var {expr} = getFocusExpr(addFocus(goUp1(state), 'body'));
        if(expr.body !== undefined)
          return goDownLeft(goDownRight(goUp1(state)));
        return goDownRight(goUp1(state));
      case 'argname':
        return addFocus(goUp1(state), 'type');
      case 'func':
        return goDownRight(goUp1(state));
      case 'arg':
        if(lastStep(state, 2) == 'func')
          return goDownRight(goUp1(goUp1(state)));
        return state;
      case 'body':
      default:
        return state;
    }
  };

  var goUp = (state) => {
    var match = {
      type: 'body', body: 'body',
      func: 'func', arg: 'func',
    }[lastStep(state)];
    if(match === undefined) return goUp1(state);
    // remove all instances of match from the end of the path
		var focus = getEdit(state, ['focus']);
    var i = focus.length-2;
    while(focus[i] === match) i--;
    return edit(state, ['focus'], focus.slice(0,i+1));
  };

  var goDown = (state) => {
    var {expr} = getFocusExpr(state);
    switch(true){
      case expr.kind == 'app':
        return goDown(goDownLeft(state));
      case expr.kind == 'lam':
      case expr.kind == 'pi':
      case expr.kind == 'type':
        return goDownLeft(state);
      default:
        return state;
    };
  };

  var goAll = (state) => goAllTheWay(goUp1, state);
  var goHome = (state) => goAllTheWay(goLeft, state);
  var goEnd = (state) => goAllTheWay(goRight, state);

  // expression manipulations

  var extractType = (state) => {
    var {expr, ctx} = getFocusExpr(state)
    var type = Candid.typecheck(expr, ctx);
    return addEdit(state, type);
  };

  var extractExpr = (state) => {
    var {expr, ctx} = getFocusExpr(state);
    return addEdit(state, expr);
  };

	var insertExpr = (state) => replace(state, state.edits[state.edits.length-1].expr);

  var replace = (state, repl) => edit(state, ['expr', ...getFocus(state)], repl);

  var remove = (state) => {
		// remove the editor if no subexpression is focused
		if(lastStep(state) === undefined){
      var edits = state.edits.slice();
      edits.splice(state.focus, 1);
      return goPrev(update(state, ['edits'], edits))
    }

		// otherwise remove the current expression
    var last = lastStep(state);
    var {expr} = getFocusExpr(goUp1(state));
    var repl;
    switch(last){
      case 'argname':
      case 'name': repl = ''; break;
      case 'value': repl = 0; break;
      case 'type':
        repl = {'pi':1,'lam':1}[expr.kind] ? Candid.shift(-1, expr.body) : expr.body;
        break;
      case 'body': repl = expr.type; break;
      case 'func': repl = expr.arg; break;
      case 'arg': repl = expr.func; break;
      default: return state;
    }
    return replace(goUp1(state), repl);
  };

  var wrap = (state, kind, which) => {
    var {expr} = getFocusExpr(state);
		var path = getFocus(state);
    var repl, focus; 
    switch(kind + '|' + which){
      case 'pi|type':
      case 'lam|type':
        repl = {kind: kind, type: expr, body: Candid.Hole};
        focus = path.length == 1 ? 'name' : 'argname';
        break;
      case 'pi|body':
      case 'lam|body':
        repl = {kind: kind, type: Candid.Hole, body:Candid.shift(1, expr)};
        focus = path.length == 1 ? 'name' : 'argname';
        break;
      case 'type|type':
        repl = {kind: kind, type: expr, body: Candid.Hole};
        focus = path.length == 1 ? 'name' : 'body';
        break;
      case 'type|body':
        repl = {kind: kind, type: Candid.Hole, body: expr};
        focus = path.length == 1 ? 'name' : 'type';
        break;
      case 'app|func':
        repl = {kind: kind, func: expr, arg: Candid.Hole};
        focus = path.length == 1 ? 'name' : 'arg';
        break;
      case 'app|arg':
        repl = {kind: kind, func: Candid.Hole, arg: expr};
        focus = path.length == 1 ? 'name' : 'func';
        break;
      default:
        throw 'Unknown wrap kind ' + kind;
    }
    return addFocus(replace(state, repl), focus);
  }

  var save = (state) => {
    var {expr} = getFocusExpr(state);
    Candid.store(expr);
    return toggleHash(state);
  }

  var toggleHash = (state) => {
    var {expr} = getFocusExpr(state);
    if(expr.kind == 'hash')
      return replace(state, Candid.unwrap(expr));
    return replace(state, Candid.enhash(expr));
  };

  var stringClear1 = (state) => replace(state, getFocusExpr(state).expr.slice(0, -1));
  var stringClear = (state) => replace(state, '');
  var stringAppend = (state, str) => replace(state, getFocusExpr(state).expr + str);

  return {
    update: update,
		lastStep: lastStep,
    lookup: lookup,
    compose: compose,
    go: {
      prev: goPrev,
      next: goNext,
      up1: goUp1,
      up: goUp,
      down: goDown,
      name: goName,
      argName: goArgName,
      downLeft: goDownLeft,
      downRight: goDownRight,
      left: goLeft,
      right: goRight,
      all: goAll,
      home: goHome,
      end: goEnd,
    },
    expr: {
      extractType: extractType,
      extractExpr: extractExpr,
      insertExpr: insertExpr,
      toggleHash: toggleHash,
      remove: remove,
      replace: replace,
      wrap: wrap,
      save: save,
    },
    str: {
      clear1: stringClear1,
      clear: stringClear,
      append: stringAppend,
    },
  };
})();
