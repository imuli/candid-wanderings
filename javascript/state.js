/*
var state = {
  exprs: [];
  focus: [];
};
*/
var State = (() => {
  var updateArray = (state, [step, ...rest], repl, updateElem) => {
    if(step === undefined){
      if(repl.constructor == Array) return repl;
      return state;
    }
    var n = state.slice();
    n[step] = updateElem(n[step], rest, repl);
    return n;
  };

  var update = (state, [step, ...rest], repl) => {
    switch(step){
      case undefined:
        if(repl.exprs.constructor == Array
          && repl.exprs.constructor == Array
        ) return repl;
        return state;
      case 'focus':
        return {
          exprs: state.exprs,
          focus: updateArray(state.focus, rest, repl, (st, path, repl) => repl),
        };
      case 'exprs':
        return {
          exprs: updateArray(state.exprs, rest, repl, Candid.update),
          focus: state.focus,
        };
      default:
        return state;
    }
  };

  var lookup = (state, [step, ...rest]) => {
    if(state.exprs[step] === undefined) throw "Bad Path: " + step;
    return Candid.lookup(state.exprs[step], rest);
  };

  var compose = (first, then) => (state) => then(first(state));

  var goAllTheWay = (func, state) => {
    while(true){
      var newstate = func(state);
      if(newstate == state) return newstate;
      state = newstate;
    }
  };

  var goUp1 = (state) => state.focus.length <= 1 ? state : {
    focus: state.focus.slice(0,-1),
    exprs: state.exprs,
  };

  var goPrev = (state) =>
    state.focus[0] === undefined || (0|state.focus[0]) === 0 ? state
      : update(state, ['focus'], [((0|state.focus[0]) - 1).toString()]);

  var goNext = (state) =>
    state.focus[0] === undefined || (0|state.focus[0]) === state.exprs.length - 1 ? state
      : update(state, ['focus'], [((0|state.focus[0]) + 1).toString()]);

  var goName = (state) => {
    var {expr} = Candid.lookup(state.exprs, state.focus);
    switch(true){
      case expr && expr.kind == 'pi':
      case expr && expr.kind == 'lam':
      case expr && expr.kind == 'type':
      case expr && expr.kind == 'app':
      case expr && expr.kind == 'hash':
        return update(state, ['focus'], [...state.focus, 'name']);
      default:
        return state;
    }
  };

  var goArgName = (state) => {
    var {expr} = Candid.lookup(state.exprs, state.focus);
    switch(true){
      case expr && expr.kind == 'pi':
      case expr && expr.kind == 'lam':
        return update(state, ['focus'], [...state.focus, 'argname']);
      default:
        return state;
    }
  };

  var goDownLeft = (state) => {
    var {expr} = Candid.lookup(state.exprs, state.focus);
    switch(true){
      case expr && expr.kind == 'pi':
      case expr && expr.kind == 'lam':
      case expr && expr.kind == 'type':
        return update(state, ['focus'], [...state.focus, 'type']);
      case expr && expr.kind == 'app':
        return update(state, ['focus'], [...state.focus, 'func']);
      default:
        return state;
    }
  };

  var goDownRight = (state) => {
    var {expr} = Candid.lookup(state.exprs, state.focus);
    switch(true){
      case expr && expr.kind == 'pi':
      case expr && expr.kind == 'lam':
      case expr && expr.kind == 'type':
        return update(state, ['focus'], [...state.focus, 'body']);
      case expr && expr.kind == 'app':
        return update(state, ['focus'], [...state.focus, 'arg']);
      default:
        return state;
    }
  };

  var goLeft = (state) => {
    var last = state.focus[state.focus.length-1];
    switch(last){
      case 'argname':
        return update(state, ['focus', state.focus.length-1], 'name');
      case 'type':
        if(state.focus[state.focus.length-2] == 'body')
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
    var last = state.focus[state.focus.length-1];
    switch(last){
      case 'name':
        var {expr} = Candid.lookup(state.exprs, state.focus.slice(0,-1));
        var into = {
          pi: 'argname',
          lam: 'argname',
          type: 'type',
          app: 'func',
        }[expr.kind];
        return update(state, ['focus', state.focus.length-1], into);
      case 'type':
        var {expr} = Candid.lookup(state.exprs, [...state.focus.slice(0,-1), 'body']);
        if(expr.body !== undefined)
          return goDownLeft(goDownRight(goUp1(state)));
        return goDownRight(goUp1(state));
      case 'argname':
        return update(state, ['focus', state.focus.length-1], 'type');
      case 'func':
        return goDownRight(goUp1(state));
      case 'arg':
        if(state.focus[state.focus.length-2] == 'func')
            return goDownRight(goUp1(goUp1(state)));
        return state;
      case 'body':
      default:
        return state;
    }
  };

  var goUp = (state) => {
    var last = state.focus[state.focus.length-1];
    var match = {
      type: 'body', body: 'body',
      func: 'func', arg: 'func',
    }[last];
    if(match === undefined) return goUp1(state);
    // remove all instances of match from the end of the path
    var i = state.focus.length-2;
    while(state.focus[i] === match) i--;
    return update(state, ['focus'], state.focus.slice(0,i+1));
  };

  var goDown = (state) => {
    var {expr} = Candid.lookup(state.exprs, state.focus);
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
    var {expr, ctx} = lookup(state, state.focus);
    var type = Candid.typecheck(expr, ctx);
    return update(state, ['exprs'], [...state.exprs, type]);
  };

  var extractExpr = (state) => {
    var {expr, ctx} = lookup(state, state.focus);
    return update(state, ['exprs'], [...state.exprs, expr]);
  };

  var remove = (state) => {
    if(state.focus.length == 1){
      var exprs = state.exprs.slice();
      exprs.splice(0|state.focus[0], 1);
      return update(update(state, ['exprs'], exprs), ['focus'],
        0|state.focus[0] >= state.exprs.length -1 ? [(state.exprs.length-2)+''] : state.focus);
    }

    var last = state.focus[state.focus.length-1];
    var {expr} = lookup(state, state.focus.slice(0,-1));
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
      default: throw "Help! " + last;
    }
    return update(update(state,
      ['exprs', ...state.focus.slice(0,-1)], repl),
      ['focus'], state.focus.slice(0, -1)
    );
  };

  var replace = (state, repl) => update(state, ['exprs', ...state.focus], repl);
  var wrap = (state, kind, which) => {
		var expr = Candid.shift(shift, lookup(state, state.focus).expr);
		var repl, focus; 
		switch(kind + '|' + which){
			case 'pi|type':
			case 'lam|type':
				repl = {kind: kind, type: expr, body: Candid.Hole};
				focus = state.focus.length == 1 ? 'name' : 'argname';
				break;
			case 'pi|body':
			case 'lam|body':
				repl = {kind: kind, type: Candid.Hole, body:Candid.shift(1, expr)};
				focus = state.focus.length == 1 ? 'name' : 'argname';
				break;
			case 'type|type':
				repl = {kind: kind, type: expr, body: Candid.Hole};
				focus = state.focus.length == 1 ? 'name' : 'body';
				break;
			case 'type|body':
				repl = {kind: kind, type: Candid.Hole, body: expr};
				focus = state.focus.length == 1 ? 'name' : 'type';
				break;
			case 'app|func':
				repl = {kind: kind, func: expr, arg: Candid.Hole};
				focus = state.focus.length == 1 ? 'name' : 'arg';
				break;
			case 'app|arg':
				repl = {kind: kind, func: Candid.Hole, arg: expr};
				focus = state.focus.length == 1 ? 'name' : 'func';
				break;
			default:
				throw 'Unknown wrap kind ' + kind;
		}
		return update(update(state,
			['exprs', ...state.focus], repl),
			['focus'], [...state.focus, focus]);
	}

  var toggleHash = (state) => {
    var {expr} = lookup(state, state.focus);
    if(expr.kind == 'hash')
      return replace(state, Candid.unwrap(expr));
    return replace(state, Candid.enhash(expr));
  };

  var stringClear1 = (state) => update(state, ['exprs', ...state.focus],
    lookup(state, state.focus).expr.slice(0,-1));
  var stringClear = (state) => update(state, ['exprs', ...state.focus], '');
  var stringAppend = (state, str) => update(state, ['exprs', ...state.focus],
    lookup(state, state.focus).expr + str);


  return {
    update: update,
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
      toggleHash: toggleHash,
      remove: remove,
      replace: replace,
    },
    str: {
      clear1: stringClear1,
      clear: stringClear,
      append: stringAppend,
    },
  };
})();
