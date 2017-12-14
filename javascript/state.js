/*
var state = {
  exprs: [];
  focus: [];
};
*/
var State = (() => {
  var updateArray = (state, [step, ...rest], repl, updateElem) => {
    switch(typeof step){
      case 'undefined':
        if(repl.constructor == Array
        ) return repl;
        return state;
      case 'number':
        var n = state.slice();
        n[step] = updateElem(n[step], rest, repl);
        return n;
      default:
        return state;
    }
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

  var goUp = (state) => ({
    focus: state.focus.slice(0,-1),
    exprs: state.exprs,
  });

  var goPrev = (state) =>
    state.focus[0] === undefined || state.focus[0] === 0 ? state
      : update(state, ['focus'], [state.focus[0] - 1]);

  var goNext = (state) =>
    state.focus[0] === undefined || state.focus[0] === state.exprs.length - 1 ? state
      : update(state, ['focus'], [state.focus[0] + 1]);

  var goName = (state) => {
    var expr = follow(state.exprs, state.focus);
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
    var expr = follow(state.exprs, state.focus);
    switch(true){
      case expr && expr.kind == 'pi':
      case expr && expr.kind == 'lam':
      case expr && expr.kind == 'type':
        return update(state, ['focus'], [...state.focus, 'argname']);
      default:
        return state;
    }
  };

  var goDownLeft = (state) => {
    var expr = follow(state.exprs, state.focus);
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
    var expr = follow(state.exprs, state.focus);
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
    var expr = follow(state.exprs, state.focus.slice(0,-1));
    var last = state.focus[state.focus.length-1];
    if(expr == undefined || last == undefined) return state;
    switch(true){
      case expr.kind == 'ref':
      case expr.kind == 'rec':
      case expr.kind == 'hole':
      case expr.kind == 'hash':
      case expr.kind == 'star':
      case last == 'name':
      case last == 'func':
      case last == 'type':
        return goAllTheWay(goDownRight, goLeft(goUp(state)));

      case last == 'argname':
        return update(state, ['focus', state.focus.length-1], 'name');

      case last == 'body':
        return update(state, ['focus', state.focus.length-1], 'type');

      case last == 'arg':
        return update(state, ['focus', state.focus.length-1], 'func');

      default:
        return state;
    }
  };

  var goRight = (state) => {
    var expr = follow(state.exprs, state.focus.slice(0,-1));
    var last = state.focus[state.focus.length-1];
    if(expr == undefined || last == undefined) return state;
    switch(true){
      case expr.kind == 'ref':
      case expr.kind == 'rec':
      case expr.kind == 'hole':
      case expr.kind == 'hash':
      case expr.kind == 'star':
      case last == 'body':
      case last == 'arg':
        return goAllTheWay(goDownLeft, goRight(goUp(state)));

      case last == 'name' && expr.kind == 'pi':
      case last == 'name' && expr.kind == 'lam':
        return update(state, ['focus', state.focus.length-1], 'argname');

      case last == 'name':
      case last == 'argname':
        return goDownLeft(goUp(state));

      case last == 'type':
        return update(state, ['focus', state.focus.length-1], 'body');

      case last == 'func':
        return update(state, ['focus', state.focus.length-1], 'arg');

      default:
        return state;
    }
  };

  return {
    update: update,
    go: {
      prev: goPrev,
      next: goNext,
      up: goUp,
      name: goName,
      argName: goArgName,
      downLeft: goDownLeft,
      downRight: goDownRight,
      left: goLeft
      right: goRight
    },
  };
})();
