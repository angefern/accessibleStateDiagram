(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.Y.I === region.ac.I)
	{
		return 'on line ' + region.Y.I;
	}
	return 'on lines ' + region.Y.I + ' through ' + region.ac.I;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aO,
		impl.aW,
		impl.aU,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		r: func(record.r),
		Z: record.Z,
		V: record.V
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.r;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.Z;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.V) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aO,
		impl.aW,
		impl.aU,
		function(sendToApp, initialModel) {
			var view = impl.aX;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aO,
		impl.aW,
		impl.aU,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.X && impl.X(sendToApp)
			var view = impl.aX;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.aF);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.aV) && (_VirtualDom_doc.title = title = doc.aV);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.aQ;
	var onUrlRequest = impl.aR;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		X: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.as === next.as
							&& curr.af === next.af
							&& curr.ap.a === next.ap.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		aO: function(flags)
		{
			return A3(impl.aO, flags, _Browser_getUrl(), key);
		},
		aX: impl.aX,
		aW: impl.aW,
		aU: impl.aU
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { aM: 'hidden', aG: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { aM: 'mozHidden', aG: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { aM: 'msHidden', aG: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { aM: 'webkitHidden', aG: 'webkitvisibilitychange' }
		: { aM: 'hidden', aG: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		ax: _Browser_getScene(),
		aA: {
			aB: _Browser_window.pageXOffset,
			aC: _Browser_window.pageYOffset,
			aY: _Browser_doc.documentElement.clientWidth,
			aK: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		aY: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		aK: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			ax: {
				aY: node.scrollWidth,
				aK: node.scrollHeight
			},
			aA: {
				aB: node.scrollLeft,
				aC: node.scrollTop,
				aY: node.clientWidth,
				aK: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			ax: _Browser_getScene(),
			aA: {
				aB: x,
				aC: y,
				aY: _Browser_doc.documentElement.clientWidth,
				aK: _Browser_doc.documentElement.clientHeight
			},
			aI: {
				aB: x + rect.left,
				aC: y + rect.top,
				aY: rect.width,
				aK: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $author$project$Main$NoOp = {$: 17};
var $author$project$Main$Tick = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $MacCASOutreach$graphicsvg$GraphicSVG$App$InitTime = function (a) {
	return {$: 1, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$App$UserMsg = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Graphics = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.d) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.e),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.e);
		} else {
			var treeLen = builder.d * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.f) : builder.f;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.d);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.e) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.e);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{f: nodeList, d: (len / $elm$core$Array$branchFactor) | 0, e: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {ae: fragment, af: host, an: path, ap: port_, as: protocol, at: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$application = _Browser_application;
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Basics$not = _Basics_not;
var $MacCASOutreach$graphicsvg$GraphicSVG$convertCoords = F2(
	function (_v0, gModel) {
		var x = _v0.a;
		var y = _v0.b;
		var sw = gModel.O;
		var sh = gModel.N;
		var cw = gModel.B;
		var ch = gModel.A;
		var aspectout = (!(!sh)) ? (sw / sh) : (4 / 3);
		var aspectin = (!(!ch)) ? (cw / ch) : (4 / 3);
		var scaledInX = _Utils_cmp(aspectout, aspectin) < 0;
		var scaledInY = _Utils_cmp(aspectout, aspectin) > 0;
		var cscale = scaledInX ? (sw / cw) : (scaledInY ? (sh / ch) : 1);
		return _Utils_Tuple2((x - (sw / 2)) / cscale, (y + (sh / 2)) / cscale);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$NoOp = {$: 3};
var $MacCASOutreach$graphicsvg$GraphicSVG$WindowResize = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			A2(
				$elm$core$Task$onError,
				A2(
					$elm$core$Basics$composeL,
					A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
					$elm$core$Result$Err),
				A2(
					$elm$core$Task$andThen,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Ok),
					task)));
	});
var $elm$browser$Browser$Dom$getViewportOf = _Browser_getViewportOf;
var $elm$core$Basics$round = _Basics_round;
var $MacCASOutreach$graphicsvg$GraphicSVG$getViewportSize = A2(
	$elm$core$Task$attempt,
	function (rvp) {
		if (!rvp.$) {
			var vp = rvp.a;
			return $MacCASOutreach$graphicsvg$GraphicSVG$WindowResize(
				$elm$core$Maybe$Just(
					_Utils_Tuple2(
						$elm$core$Basics$round(vp.aA.aY),
						$elm$core$Basics$round(vp.aA.aK))));
		} else {
			return $MacCASOutreach$graphicsvg$GraphicSVG$NoOp;
		}
	},
	$elm$browser$Browser$Dom$getViewportOf('render'));
var $elm$core$Platform$Cmd$map = _Platform_map;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $MacCASOutreach$graphicsvg$GraphicSVG$hiddenAppUpdate = F4(
	function (userView, userUpdate, msg, _v0) {
		var userModel = _v0.a;
		var gModel = _v0.b;
		var mapUserCmd = function (cmd) {
			return A2($elm$core$Platform$Cmd$map, $MacCASOutreach$graphicsvg$GraphicSVG$Graphics, cmd);
		};
		var _v1 = userView(userModel).aF;
		var cw = _v1.a;
		var ch = _v1.b;
		switch (msg.$) {
			case 0:
				var message = msg.a;
				var _v3 = A2(userUpdate, message, userModel);
				var newModel = _v3.a;
				var userCmds = _v3.b;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						newModel,
						_Utils_update(
							gModel,
							{A: ch, B: cw})),
					mapUserCmd(userCmds));
			case 1:
				var mWH = msg.a;
				if (!mWH.$) {
					var _v5 = mWH.a;
					var w = _v5.a;
					var h = _v5.b;
					return _Utils_Tuple2(
						_Utils_Tuple2(
							userModel,
							_Utils_update(
								gModel,
								{N: h, O: w})),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_Tuple2(userModel, gModel),
						$MacCASOutreach$graphicsvg$GraphicSVG$getViewportSize);
				}
			case 2:
				var message = msg.a;
				var _v6 = msg.b;
				var x = _v6.a;
				var y = _v6.b;
				var _v7 = A2(
					userUpdate,
					message(
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$convertCoords,
							_Utils_Tuple2(x, y),
							gModel)),
					userModel);
				var newModel = _v7.a;
				var userCmds = _v7.b;
				return _Utils_Tuple2(
					_Utils_Tuple2(newModel, gModel),
					mapUserCmd(userCmds));
			default:
				return _Utils_Tuple2(
					_Utils_Tuple2(userModel, gModel),
					$elm$core$Platform$Cmd$none);
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$ReturnPosition = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$clipPath = $elm$svg$Svg$trustedNode('clipPath');
var $elm$svg$Svg$defs = $elm$svg$Svg$trustedNode('defs');
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $MacCASOutreach$graphicsvg$GraphicSVG$cPath = F2(
	function (w, h) {
		return A2(
			$elm$svg$Svg$defs,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$clipPath,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$id('cPath')
						]),
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$rect,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$width(
									$elm$core$String$fromFloat(w)),
									$elm$svg$Svg$Attributes$height(
									$elm$core$String$fromFloat(h)),
									$elm$svg$Svg$Attributes$x(
									$elm$core$String$fromFloat((-w) / 2)),
									$elm$svg$Svg$Attributes$y(
									$elm$core$String$fromFloat((-h) / 2))
								]),
							_List_Nil)
						]))
				]));
	});
var $elm$svg$Svg$Attributes$clipPath = _VirtualDom_attribute('clip-path');
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Everything = {$: 11};
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Group = function (a) {
	return {$: 7, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Notathing = {$: 12};
var $elm$svg$Svg$a = $elm$svg$Svg$trustedNode('a');
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Solid = function (a) {
	return {$: 0, a: a};
};
var $avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $avh4$elm_color$Color$fromRgba = function (components) {
	return A4($avh4$elm_color$Color$RgbaSpace, components.W, components.S, components.R, components.z);
};
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$ssa = function (n) {
	return A3($elm$core$Basics$clamp, 0, 1, n);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$ssc = function (n) {
	return A3($elm$core$Basics$clamp, 0, 255, n);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$rgba = F4(
	function (r, g, b, a) {
		return $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Solid(
			$avh4$elm_color$Color$fromRgba(
				{
					z: $MacCASOutreach$graphicsvg$GraphicSVG$ssa(a),
					R: $MacCASOutreach$graphicsvg$GraphicSVG$ssc(b),
					S: $MacCASOutreach$graphicsvg$GraphicSVG$ssc(g),
					W: $MacCASOutreach$graphicsvg$GraphicSVG$ssc(r)
				}));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$black = A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 0, 0, 0, 1);
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$contenteditable = $elm$html$Html$Attributes$boolProperty('contentEditable');
var $MacCASOutreach$graphicsvg$GraphicSVG$pairToString = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return $elm$core$String$fromFloat(x) + (',' + $elm$core$String$fromFloat(y));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$bezierStringHelper = function (_v0) {
	var _v1 = _v0.a;
	var a = _v1.a;
	var b = _v1.b;
	var _v2 = _v0.b;
	var c = _v2.a;
	var d = _v2.b;
	return ' Q ' + ($MacCASOutreach$graphicsvg$GraphicSVG$pairToString(
		_Utils_Tuple2(a, b)) + (' ' + $MacCASOutreach$graphicsvg$GraphicSVG$pairToString(
		_Utils_Tuple2(c, d))));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$createBezierString = F2(
	function (first, list) {
		return 'M ' + ($MacCASOutreach$graphicsvg$GraphicSVG$pairToString(first) + $elm$core$String$concat(
			A2($elm$core$List$map, $MacCASOutreach$graphicsvg$GraphicSVG$bezierStringHelper, list)));
	});
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $elm$svg$Svg$Attributes$gradientTransform = _VirtualDom_attribute('gradientTransform');
var $elm$svg$Svg$Attributes$gradientUnits = _VirtualDom_attribute('gradientUnits');
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$svg$Svg$linearGradient = $elm$svg$Svg$trustedNode('linearGradient');
var $elm$svg$Svg$Attributes$mask = _VirtualDom_attribute('mask');
var $avh4$elm_color$Color$toRgba = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	return {z: a, R: b, S: g, W: r};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$mkRGB = function (colour) {
	var col = $avh4$elm_color$Color$toRgba(colour);
	return 'rgba(' + ($elm$core$String$fromFloat(col.W) + (',' + ($elm$core$String$fromFloat(col.S) + (',' + ($elm$core$String$fromFloat(col.R) + (',' + ($elm$core$String$fromFloat(col.z) + ')')))))));
};
var $elm$svg$Svg$Attributes$offset = _VirtualDom_attribute('offset');
var $elm$core$Basics$pi = _Basics_pi;
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $elm$svg$Svg$radialGradient = $elm$svg$Svg$trustedNode('radialGradient');
var $elm$svg$Svg$stop = $elm$svg$Svg$trustedNode('stop');
var $elm$svg$Svg$Attributes$style = _VirtualDom_attribute('style');
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $MacCASOutreach$graphicsvg$GraphicSVG$createGradientSVG = F3(
	function (id, _v0, grad) {
		var wid = _v0.a;
		var hei = _v0.b;
		var w = function () {
			if (!grad.$) {
				var stops = grad.a;
				var _v6 = $elm$core$List$head(
					$elm$core$List$reverse(stops));
				if (!_v6.$) {
					var _v7 = _v6.a;
					var pos = _v7.a;
					return pos;
				} else {
					return 0;
				}
			} else {
				var stops = grad.b;
				var _v8 = $elm$core$List$head(
					$elm$core$List$reverse(stops));
				if (!_v8.$) {
					var _v9 = _v8.a;
					var pos = _v9.a;
					return pos;
				} else {
					return 0;
				}
			}
		}();
		var squareSize = (_Utils_cmp(wid, hei) > 0) ? (2 * wid) : (2 * hei);
		var rotation = function () {
			if (grad.$ === 1) {
				var rot = grad.a;
				var stops = grad.b;
				return (rot * 180) / $elm$core$Basics$pi;
			} else {
				return 0;
			}
		}();
		var isRadial = function () {
			if (!grad.$) {
				return true;
			} else {
				return false;
			}
		}();
		var createStop = function (_v2) {
			var pos = _v2.a;
			var trans = _v2.b;
			var colour = _v2.c;
			var start = isRadial ? 0 : (((1 - (w / squareSize)) / 2) * 100);
			var percent = isRadial ? ((pos / w) * 100) : (start + ((pos / squareSize) * 100));
			var percentTxt = $elm$core$String$fromFloat(percent) + '%';
			var opacityTxt = 'stop-opacity:' + ($elm$core$String$fromFloat(trans) + ';');
			var colourTxt = 'stop-color:' + ($MacCASOutreach$graphicsvg$GraphicSVG$mkRGB(colour) + ';');
			return A2(
				$elm$svg$Svg$stop,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$offset(percentTxt),
						$elm$svg$Svg$Attributes$style(
						_Utils_ap(colourTxt, opacityTxt))
					]),
				_List_Nil);
		};
		var defs = A2(
			$elm$svg$Svg$defs,
			_List_Nil,
			_List_fromArray(
				[
					function () {
					if (grad.$ === 1) {
						var stops = grad.b;
						return A2(
							$elm$svg$Svg$linearGradient,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id(id + 'gradient'),
									$elm$svg$Svg$Attributes$gradientTransform(
									'rotate(' + ($elm$core$String$fromFloat(rotation) + 'rad)')),
									$elm$svg$Svg$Attributes$gradientTransform(
									'rotate(' + ($elm$core$String$fromFloat(rotation) + 'rad)'))
								]),
							A2($elm$core$List$map, createStop, stops));
					} else {
						var stops = grad.a;
						return A2(
							$elm$svg$Svg$radialGradient,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id(id + 'gradient'),
									$elm$svg$Svg$Attributes$cx('0'),
									$elm$svg$Svg$Attributes$cy('0'),
									$elm$svg$Svg$Attributes$r(
									$elm$core$String$fromFloat(w)),
									$elm$svg$Svg$Attributes$gradientUnits('userSpaceOnUse')
								]),
							A2($elm$core$List$map, createStop, stops));
					}
				}()
				]));
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$mask('url(#' + (id + 'mask)'))
				]),
			_List_fromArray(
				[
					defs,
					A2(
					$elm$svg$Svg$rect,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x(
							$elm$core$String$fromFloat((-squareSize) / 2)),
							$elm$svg$Svg$Attributes$y(
							$elm$core$String$fromFloat((-squareSize) / 2)),
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromFloat(squareSize)),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromFloat(squareSize)),
							$elm$svg$Svg$Attributes$fill('url(#' + (id + 'gradient)')),
							$elm$svg$Svg$Attributes$id(id + 'grad'),
							$elm$svg$Svg$Attributes$transform(
							'rotate(' + ($elm$core$String$fromFloat(rotation) + ')'))
						]),
					_List_Nil)
				]));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Inked = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$filled = F2(
	function (color, stencil) {
		return A3(
			$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Inked,
			$elm$core$Maybe$Just(color),
			$elm$core$Maybe$Nothing,
			stencil);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Move = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$move = F2(
	function (disp, shape) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$Secret$Move, disp, shape);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Rect = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$rect = F2(
	function (w, h) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$Secret$Rect, w, h);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$createGraphX = F5(
	function (h, s, th, c, x) {
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(x * s, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				c,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, th, h)));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$createGraphY = F5(
	function (w, s, th, c, y) {
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, y * s),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				c,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, th)));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$group = function (shapes) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Group(shapes);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$createGraph = F4(
	function (_v0, s, th, c) {
		var w = _v0.a;
		var h = _v0.b;
		var syi = $elm$core$Basics$ceiling(h / (s * 2));
		var ylisti = A2($elm$core$List$range, -syi, syi);
		var sxi = $elm$core$Basics$ceiling(w / (s * 2));
		var xlisti = A2($elm$core$List$range, -sxi, sxi);
		return $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_Utils_ap(
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeL,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$createGraphX, h, s, th, c),
						$elm$core$Basics$toFloat),
					xlisti),
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeL,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$createGraphY, w, s, th, c),
						$elm$core$Basics$toFloat),
					ylisti)));
	});
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$ellipse = $elm$svg$Svg$trustedNode('ellipse');
var $elm$svg$Svg$Attributes$fillOpacity = _VirtualDom_attribute('fill-opacity');
var $elm$svg$Svg$Attributes$fontSize = _VirtualDom_attribute('font-size');
var $elm$svg$Svg$foreignObject = $elm$svg$Svg$trustedNode('foreignObject');
var $MacCASOutreach$graphicsvg$GraphicSVG$getAlpha = function (colour) {
	return $avh4$elm_color$Color$toRgba(colour).z;
};
var $MacCASOutreach$graphicsvg$GraphicSVG$ident = _Utils_Tuple2(
	_Utils_Tuple3(1, 0, 0),
	_Utils_Tuple3(0, 1, 0));
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$svg$Svg$mask = $elm$svg$Svg$trustedNode('mask');
var $MacCASOutreach$graphicsvg$GraphicSVG$matrixMult = F2(
	function (_v0, _v3) {
		var _v1 = _v0.a;
		var a = _v1.a;
		var c = _v1.b;
		var e = _v1.c;
		var _v2 = _v0.b;
		var b = _v2.a;
		var d = _v2.b;
		var f = _v2.c;
		var _v4 = _v3.a;
		var a1 = _v4.a;
		var c1 = _v4.b;
		var e1 = _v4.c;
		var _v5 = _v3.b;
		var b1 = _v5.a;
		var d1 = _v5.b;
		var f1 = _v5.c;
		return _Utils_Tuple2(
			_Utils_Tuple3((a * a1) + (c * b1), (a * c1) + (c * d1), (e + (a * e1)) + (c * f1)),
			_Utils_Tuple3((b * a1) + (d * b1), (b * c1) + (d * d1), (f + (b * e1)) + (d * f1)));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$mkAlpha = A2($elm$core$Basics$composeL, $elm$core$String$fromFloat, $MacCASOutreach$graphicsvg$GraphicSVG$getAlpha);
var $MacCASOutreach$graphicsvg$GraphicSVG$moveT = F2(
	function (_v0, _v1) {
		var u = _v0.a;
		var v = _v0.b;
		var _v2 = _v1.a;
		var a = _v2.a;
		var c = _v2.b;
		var tx = _v2.c;
		var _v3 = _v1.b;
		var b = _v3.a;
		var d = _v3.b;
		var ty = _v3.c;
		return _Utils_Tuple2(
			_Utils_Tuple3(a, c, (tx + (a * u)) + (c * v)),
			_Utils_Tuple3(b, d, (ty + (b * u)) + (d * v)));
	});
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (x, y) {
			return _Utils_Tuple2(x, -y);
		}),
	A2($elm$json$Json$Decode$field, 'offsetX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'offsetY', $elm$json$Json$Decode$float));
var $MacCASOutreach$graphicsvg$GraphicSVG$onEnterAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseover',
		A2($elm$json$Json$Decode$map, msg, $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onLeaveAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseleave',
		A2($elm$json$Json$Decode$map, msg, $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder));
};
var $elm$html$Html$Events$onMouseDown = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousedown',
		$elm$json$Json$Decode$succeed(msg));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onMouseDownAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousedown',
		A2($elm$json$Json$Decode$map, msg, $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder));
};
var $elm$html$Html$Events$onMouseEnter = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseenter',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onMouseLeave = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseleave',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onMouseUp = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseup',
		$elm$json$Json$Decode$succeed(msg));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onMouseUpAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseup',
		A2($elm$json$Json$Decode$map, msg, $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onMoveAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousemove',
		A2($elm$json$Json$Decode$map, msg, $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onTapAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		A2($elm$json$Json$Decode$map, msg, $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onTouchEnd = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'touchend',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$TouchPos = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $MacCASOutreach$graphicsvg$GraphicSVG$touchDecoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['touches', '0']),
			A3(
				$elm$json$Json$Decode$map2,
				$MacCASOutreach$graphicsvg$GraphicSVG$TouchPos,
				A2($elm$json$Json$Decode$field, 'pageX', $elm$json$Json$Decode$float),
				A2($elm$json$Json$Decode$field, 'pageY', $elm$json$Json$Decode$float))),
			A3(
			$elm$json$Json$Decode$map2,
			$MacCASOutreach$graphicsvg$GraphicSVG$TouchPos,
			A2($elm$json$Json$Decode$field, 'pageX', $elm$json$Json$Decode$float),
			A2($elm$json$Json$Decode$field, 'pageY', $elm$json$Json$Decode$float))
		]));
var $MacCASOutreach$graphicsvg$GraphicSVG$touchToPair = function (tp) {
	var x = tp.a;
	var y = tp.b;
	return _Utils_Tuple2(x, -y);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onTouchMove = function (msg) {
	return A2(
		$elm$html$Html$Events$preventDefaultOn,
		'touchmove',
		A2(
			$elm$json$Json$Decode$map,
			function (a) {
				return _Utils_Tuple2(
					A2($elm$core$Basics$composeL, msg, $MacCASOutreach$graphicsvg$GraphicSVG$touchToPair)(a),
					true);
			},
			$MacCASOutreach$graphicsvg$GraphicSVG$touchDecoder));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onTouchStart = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'touchstart',
		$elm$json$Json$Decode$succeed(msg));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onTouchStartAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'touchstart',
		A2(
			$elm$json$Json$Decode$map,
			A2($elm$core$Basics$composeL, msg, $MacCASOutreach$graphicsvg$GraphicSVG$touchToPair),
			$MacCASOutreach$graphicsvg$GraphicSVG$touchDecoder));
};
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $elm$svg$Svg$polygon = $elm$svg$Svg$trustedNode('polygon');
var $elm$svg$Svg$polyline = $elm$svg$Svg$trustedNode('polyline');
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$AlphaMask = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Clip = F2(
	function (a, b) {
		return {$: 10, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$EnterAt = F2(
	function (a, b) {
		return {$: 17, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$EnterShape = F2(
	function (a, b) {
		return {$: 16, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Exit = F2(
	function (a, b) {
		return {$: 18, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$ExitAt = F2(
	function (a, b) {
		return {$: 19, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$ForeignObject = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$GraphPaper = F3(
	function (a, b, c) {
		return {$: 30, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$GroupOutline = function (a) {
	return {$: 8, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Link = F2(
	function (a, b) {
		return {$: 13, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$MouseDown = F2(
	function (a, b) {
		return {$: 20, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$MouseDownAt = F2(
	function (a, b) {
		return {$: 21, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$MouseUp = F2(
	function (a, b) {
		return {$: 22, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$MouseUpAt = F2(
	function (a, b) {
		return {$: 23, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$MoveOverAt = F2(
	function (a, b) {
		return {$: 24, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Rotate = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Scale = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Skew = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Tap = F2(
	function (a, b) {
		return {$: 14, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$TapAt = F2(
	function (a, b) {
		return {$: 15, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchEnd = F2(
	function (a, b) {
		return {$: 26, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchEndAt = F2(
	function (a, b) {
		return {$: 28, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchMoveAt = F2(
	function (a, b) {
		return {$: 29, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchStart = F2(
	function (a, b) {
		return {$: 25, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchStartAt = F2(
	function (a, b) {
		return {$: 27, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Transformed = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$repaint = F2(
	function (color, shape) {
		switch (shape.$) {
			case 0:
				if (shape.b.$ === 1) {
					var _v1 = shape.b;
					var st = shape.c;
					return A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Inked,
						$elm$core$Maybe$Just(color),
						$elm$core$Maybe$Nothing,
						st);
				} else {
					var _v2 = shape.b.a;
					var lt = _v2.a;
					var st = shape.c;
					return A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Inked,
						$elm$core$Maybe$Just(color),
						$elm$core$Maybe$Just(
							_Utils_Tuple2(lt, color)),
						st);
				}
			case 2:
				var s = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Move,
					s,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 3:
				var r = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Rotate,
					r,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 4:
				var sx = shape.a;
				var sy = shape.b;
				var sh = shape.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Scale,
					sx,
					sy,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 5:
				var skx = shape.a;
				var sky = shape.b;
				var sh = shape.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Skew,
					skx,
					sky,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 6:
				var tm = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Transformed,
					tm,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 7:
				var shapes = shape.a;
				return $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Group(
					A2(
						$elm$core$List$map,
						$MacCASOutreach$graphicsvg$GraphicSVG$repaint(color),
						shapes));
			case 8:
				var cmbndshp = shape.a;
				return $MacCASOutreach$graphicsvg$GraphicSVG$Secret$GroupOutline(
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, cmbndshp));
			case 13:
				var s = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Link,
					s,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 9:
				var reg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$AlphaMask,
					reg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 10:
				var reg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Clip,
					reg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 14:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Tap,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 15:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$TapAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 16:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$EnterShape,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 17:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$EnterAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 18:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Exit,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 19:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$ExitAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 20:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$MouseDown,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 21:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$MouseDownAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 22:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$MouseUp,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 23:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$MouseUpAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 24:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$MoveOverAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 25:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchStart,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 26:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchEnd,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 27:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchStartAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 28:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchEndAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 29:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchMoveAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 1:
				var w = shape.a;
				var h = shape.b;
				var htm = shape.c;
				return A3($MacCASOutreach$graphicsvg$GraphicSVG$Secret$ForeignObject, w, h, htm);
			case 11:
				return $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Everything;
			case 12:
				return $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Notathing;
			default:
				var s = shape.a;
				var th = shape.b;
				return A3($MacCASOutreach$graphicsvg$GraphicSVG$Secret$GraphPaper, s, th, color);
		}
	});
var $avh4$elm_color$Color$rgb = F3(
	function (r, g, b) {
		return A4($avh4$elm_color$Color$RgbaSpace, r, g, b, 1.0);
	});
var $elm$core$Basics$cos = _Basics_cos;
var $elm$core$Basics$sin = _Basics_sin;
var $MacCASOutreach$graphicsvg$GraphicSVG$rotateT = F2(
	function (rad, _v0) {
		var _v1 = _v0.a;
		var a = _v1.a;
		var c = _v1.b;
		var tx = _v1.c;
		var _v2 = _v0.b;
		var b = _v2.a;
		var d = _v2.b;
		var ty = _v2.c;
		var sinX = $elm$core$Basics$sin(rad);
		var cosX = $elm$core$Basics$cos(rad);
		return _Utils_Tuple2(
			_Utils_Tuple3((a * cosX) + (c * sinX), (c * cosX) - (a * sinX), tx),
			_Utils_Tuple3((b * cosX) + (d * sinX), (d * cosX) - (b * sinX), ty));
	});
var $elm$svg$Svg$Attributes$rx = _VirtualDom_attribute('rx');
var $elm$svg$Svg$Attributes$ry = _VirtualDom_attribute('ry');
var $MacCASOutreach$graphicsvg$GraphicSVG$scaleT = F3(
	function (sx, sy, _v0) {
		var _v1 = _v0.a;
		var a = _v1.a;
		var c = _v1.b;
		var tx = _v1.c;
		var _v2 = _v0.b;
		var b = _v2.a;
		var d = _v2.b;
		var ty = _v2.c;
		return _Utils_Tuple2(
			_Utils_Tuple3(a * sx, c * sy, tx),
			_Utils_Tuple3(b * sx, d * sy, ty));
	});
var $elm$core$Basics$tan = _Basics_tan;
var $MacCASOutreach$graphicsvg$GraphicSVG$skewT = F3(
	function (skx, sky, _v0) {
		var _v1 = _v0.a;
		var a = _v1.a;
		var c = _v1.b;
		var tx = _v1.c;
		var _v2 = _v0.b;
		var b = _v2.a;
		var d = _v2.b;
		var ty = _v2.c;
		var tanY = $elm$core$Basics$tan(-sky);
		var tanX = $elm$core$Basics$tan(-skx);
		return _Utils_Tuple2(
			_Utils_Tuple3(a + (c * tanY), c + (a * tanX), tx),
			_Utils_Tuple3(b + (d * tanY), d + (b * tanX), ty));
	});
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeDasharray = _VirtualDom_attribute('stroke-dasharray');
var $elm$svg$Svg$Attributes$strokeOpacity = _VirtualDom_attribute('stroke-opacity');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$Attributes$target = _VirtualDom_attribute('target');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $elm$svg$Svg$Attributes$textAnchor = _VirtualDom_attribute('text-anchor');
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $MacCASOutreach$graphicsvg$GraphicSVG$white = A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 1);
var $elm$svg$Svg$Attributes$xlinkHref = function (value) {
	return A3(
		_VirtualDom_attributeNS,
		'http://www.w3.org/1999/xlink',
		'xlink:href',
		_VirtualDom_noJavaScriptUri(value));
};
var $elm$svg$Svg$Attributes$xmlSpace = A2(_VirtualDom_attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:space');
var $MacCASOutreach$graphicsvg$GraphicSVG$createSVG = F7(
	function (id, w, h, trans, msgWrapper, positionWrapper, shape) {
		createSVG:
		while (true) {
			switch (shape.$) {
				case 0:
					var fillClr = shape.a;
					var lt = shape.b;
					var stencil = shape.c;
					var strokeAttrs = function () {
						_v13$3:
						while (true) {
							if (lt.$ === 1) {
								return _List_Nil;
							} else {
								if (!lt.a.b.$) {
									switch (lt.a.a.$) {
										case 1:
											var _v14 = lt.a;
											var th = _v14.a.a;
											var strokeClr = _v14.b.a;
											var nonStroke = function () {
												var opcty = $MacCASOutreach$graphicsvg$GraphicSVG$getAlpha(strokeClr);
												return (th <= 0) || (opcty <= 0);
											}();
											return nonStroke ? _List_Nil : _List_fromArray(
												[
													$elm$svg$Svg$Attributes$strokeWidth(
													$elm$core$String$fromFloat(th)),
													$elm$svg$Svg$Attributes$stroke(
													$MacCASOutreach$graphicsvg$GraphicSVG$mkRGB(strokeClr)),
													$elm$svg$Svg$Attributes$strokeOpacity(
													$MacCASOutreach$graphicsvg$GraphicSVG$mkAlpha(strokeClr))
												]);
										case 2:
											var _v15 = lt.a;
											var _v16 = _v15.a;
											var dashes = _v16.a;
											var th = _v16.b;
											var strokeClr = _v15.b.a;
											var nonStroke = function () {
												var opcty = $MacCASOutreach$graphicsvg$GraphicSVG$getAlpha(strokeClr);
												return (th <= 0) || ((opcty <= 0) || A2(
													$elm$core$List$all,
													function (_v17) {
														var on = _v17.a;
														return !on;
													},
													dashes));
											}();
											return nonStroke ? _List_Nil : _Utils_ap(
												_List_fromArray(
													[
														$elm$svg$Svg$Attributes$strokeWidth(
														$elm$core$String$fromFloat(th)),
														$elm$svg$Svg$Attributes$stroke(
														$MacCASOutreach$graphicsvg$GraphicSVG$mkRGB(strokeClr)),
														$elm$svg$Svg$Attributes$strokeOpacity(
														$MacCASOutreach$graphicsvg$GraphicSVG$mkAlpha(strokeClr))
													]),
												_List_fromArray(
													[
														$elm$svg$Svg$Attributes$strokeDasharray(
														$elm$core$String$concat(
															A2(
																$elm$core$List$intersperse,
																',',
																A2($elm$core$List$map, $MacCASOutreach$graphicsvg$GraphicSVG$pairToString, dashes))))
													]));
										default:
											break _v13$3;
									}
								} else {
									break _v13$3;
								}
							}
						}
						var _v18 = lt.a;
						return _List_Nil;
					}();
					var nonexistBody = function () {
						if (fillClr.$ === 1) {
							return true;
						} else {
							return false;
						}
					}();
					var gradientDefs = function () {
						if ((!fillClr.$) && (fillClr.a.$ === 1)) {
							var gr = fillClr.a.a;
							return $elm$core$Maybe$Just(
								A3(
									$MacCASOutreach$graphicsvg$GraphicSVG$createGradientSVG,
									id,
									_Utils_Tuple2(w, h),
									gr));
						} else {
							return $elm$core$Maybe$Nothing;
						}
					}();
					var clrAttrs = function () {
						if (fillClr.$ === 1) {
							return _List_fromArray(
								[
									$elm$svg$Svg$Attributes$fill('none')
								]);
						} else {
							if (!fillClr.a.$) {
								var bodyClr = fillClr.a.a;
								return _List_fromArray(
									[
										$elm$svg$Svg$Attributes$fill(
										$MacCASOutreach$graphicsvg$GraphicSVG$mkRGB(bodyClr)),
										$elm$svg$Svg$Attributes$fillOpacity(
										$MacCASOutreach$graphicsvg$GraphicSVG$mkAlpha(bodyClr))
									]);
							} else {
								return _List_fromArray(
									[
										$elm$svg$Svg$Attributes$id(id),
										$elm$svg$Svg$Attributes$fill(
										$MacCASOutreach$graphicsvg$GraphicSVG$mkRGB(
											A3($avh4$elm_color$Color$rgb, 255, 255, 255)))
									]);
							}
						}
					}();
					var _v1 = trans;
					var _v2 = _v1.a;
					var a = _v2.a;
					var c = _v2.b;
					var tx = _v2.c;
					var _v3 = _v1.b;
					var b = _v3.a;
					var d = _v3.b;
					var ty = _v3.c;
					var transAttrs = _List_fromArray(
						[
							$elm$svg$Svg$Attributes$transform(
							'matrix(' + ($elm$core$String$concat(
								A2(
									$elm$core$List$intersperse,
									',',
									A2(
										$elm$core$List$map,
										$elm$core$String$fromFloat,
										_List_fromArray(
											[a, -b, c, -d, tx, -ty])))) + ')'))
						]);
					var attrs = function () {
						if ((!fillClr.$) && (fillClr.a.$ === 1)) {
							return _Utils_ap(clrAttrs, strokeAttrs);
						} else {
							return _Utils_ap(
								transAttrs,
								_Utils_ap(clrAttrs, strokeAttrs));
						}
					}();
					var basicShape = function () {
						switch (stencil.$) {
							case 0:
								var r = stencil.a;
								return A2(
									$elm$svg$Svg$circle,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$cx('0'),
												$elm$svg$Svg$Attributes$cy('0'),
												$elm$svg$Svg$Attributes$r(
												$elm$core$String$fromFloat(r))
											]),
										attrs),
									_List_Nil);
							case 1:
								var rw = stencil.a;
								var rh = stencil.b;
								return A2(
									$elm$svg$Svg$rect,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$x(
												$elm$core$String$fromFloat((-rw) / 2)),
												$elm$svg$Svg$Attributes$y(
												$elm$core$String$fromFloat((-rh) / 2)),
												$elm$svg$Svg$Attributes$width(
												$elm$core$String$fromFloat(rw)),
												$elm$svg$Svg$Attributes$height(
												$elm$core$String$fromFloat(rh))
											]),
										attrs),
									_List_Nil);
							case 2:
								var rw = stencil.a;
								var rh = stencil.b;
								var r = stencil.c;
								return A2(
									$elm$svg$Svg$rect,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$x(
												$elm$core$String$fromFloat((-rw) / 2)),
												$elm$svg$Svg$Attributes$y(
												$elm$core$String$fromFloat((-rh) / 2)),
												$elm$svg$Svg$Attributes$rx(
												$elm$core$String$fromFloat(r)),
												$elm$svg$Svg$Attributes$ry(
												$elm$core$String$fromFloat(r)),
												$elm$svg$Svg$Attributes$width(
												$elm$core$String$fromFloat(rw)),
												$elm$svg$Svg$Attributes$height(
												$elm$core$String$fromFloat(rh))
											]),
										attrs),
									_List_Nil);
							case 3:
								var ow = stencil.a;
								var oh = stencil.b;
								return A2(
									$elm$svg$Svg$ellipse,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$cx('0'),
												$elm$svg$Svg$Attributes$cy('0'),
												$elm$svg$Svg$Attributes$rx(
												$elm$core$String$fromFloat(0.5 * ow)),
												$elm$svg$Svg$Attributes$ry(
												$elm$core$String$fromFloat(0.5 * oh))
											]),
										attrs),
									_List_Nil);
							case 5:
								var vertices = stencil.a;
								return A2(
									$elm$svg$Svg$polygon,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$points(
												$elm$core$String$concat(
													A2(
														$elm$core$List$intersperse,
														' ',
														A2($elm$core$List$map, $MacCASOutreach$graphicsvg$GraphicSVG$pairToString, vertices))))
											]),
										attrs),
									_List_Nil);
							case 6:
								var vertices = stencil.a;
								return A2(
									$elm$svg$Svg$polyline,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$points(
												$elm$core$String$concat(
													A2(
														$elm$core$List$intersperse,
														' ',
														A2($elm$core$List$map, $MacCASOutreach$graphicsvg$GraphicSVG$pairToString, vertices))))
											]),
										attrs),
									_List_Nil);
							case 4:
								var start = stencil.a;
								var pts = stencil.b;
								return A2(
									$elm$svg$Svg$path,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$d(
												A2($MacCASOutreach$graphicsvg$GraphicSVG$createBezierString, start, pts))
											]),
										attrs),
									_List_Nil);
							default:
								var _v6 = stencil.a;
								var si = _v6.a;
								var bo = _v6.b;
								var i = _v6.c;
								var u = _v6.d;
								var s = _v6.e;
								var sel = _v6.f;
								var f = _v6.g;
								var align = _v6.h;
								var str = stencil.b;
								var txtDec = (u && s) ? 'text-decoration: underline line-through;' : (u ? 'text-decoration: underline;' : (s ? 'text-decoration: line-through;' : ''));
								var select = (!sel) ? '-webkit-touch-callout: none;\n-webkit-user-select: none;\n-khtml-user-select: none;\n-moz-user-select: none;\n-ms-user-select: none;\nuser-select: none;cursor: default;' : '';
								var it = i ? 'font-style: italic;' : '';
								var font = function () {
									switch (f.$) {
										case 1:
											return 'sans-serif;';
										case 0:
											return 'serif;';
										case 2:
											return 'monospace;';
										default:
											var fStr = f.a;
											return fStr + ';';
									}
								}();
								var bol = bo ? 'font-weight: bold;' : '';
								var sty = bol + (it + (txtDec + ('font-family: ' + (font + select))));
								var anchor = function () {
									switch (align) {
										case 1:
											return 'middle';
										case 0:
											return 'start';
										default:
											return 'end';
									}
								}();
								return A2(
									$elm$svg$Svg$text_,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$x('0'),
												$elm$svg$Svg$Attributes$y('0'),
												$elm$svg$Svg$Attributes$style(sty),
												$elm$svg$Svg$Attributes$fontSize(
												$elm$core$String$fromFloat(si)),
												$elm$svg$Svg$Attributes$textAnchor(anchor),
												$elm$html$Html$Attributes$contenteditable(true)
											]),
										_Utils_ap(
											_List_fromArray(
												[
													$elm$svg$Svg$Attributes$transform(
													'matrix(' + ($elm$core$String$concat(
														A2(
															$elm$core$List$intersperse,
															',',
															A2(
																$elm$core$List$map,
																$elm$core$String$fromFloat,
																_List_fromArray(
																	[a, -b, -c, d, tx, -ty])))) + ')'))
												]),
											_Utils_ap(
												_List_fromArray(
													[
														$elm$svg$Svg$Attributes$xmlSpace('preserve')
													]),
												_Utils_ap(clrAttrs, strokeAttrs)))),
									_List_fromArray(
										[
											$elm$svg$Svg$text(str)
										]));
						}
					}();
					if (nonexistBody && $elm$core$List$isEmpty(strokeAttrs)) {
						return A2($elm$svg$Svg$g, _List_Nil, _List_Nil);
					} else {
						if (!gradientDefs.$) {
							var gDefs = gradientDefs.a;
							return A2(
								$elm$svg$Svg$g,
								transAttrs,
								_List_fromArray(
									[
										A2(
										$elm$svg$Svg$mask,
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$id(id + 'mask')
											]),
										_List_fromArray(
											[basicShape])),
										gDefs
									]));
						} else {
							return basicShape;
						}
					}
				case 1:
					var fw = shape.a;
					var fh = shape.b;
					var htm = shape.c;
					var _v19 = trans;
					var _v20 = _v19.a;
					var a = _v20.a;
					var c = _v20.b;
					var tx = _v20.c;
					var _v21 = _v19.b;
					var b = _v21.a;
					var d = _v21.b;
					var ty = _v21.c;
					return A2(
						$elm$svg$Svg$foreignObject,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$width(
								$elm$core$String$fromFloat(fw)),
								$elm$svg$Svg$Attributes$height(
								$elm$core$String$fromFloat(fh)),
								$elm$svg$Svg$Attributes$transform(
								'matrix(' + ($elm$core$String$concat(
									A2(
										$elm$core$List$intersperse,
										',',
										A2(
											$elm$core$List$map,
											$elm$core$String$fromFloat,
											_List_fromArray(
												[a, -b, -c, d, tx, -ty])))) + ')'))
							]),
						_List_fromArray(
							[
								A2($elm$html$Html$map, msgWrapper, htm)
							]));
				case 2:
					var v = shape.a;
					var sh = shape.b;
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = A2($MacCASOutreach$graphicsvg$GraphicSVG$moveT, v, trans),
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = sh;
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 11:
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = $MacCASOutreach$graphicsvg$GraphicSVG$ident,
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h));
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 12:
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = $MacCASOutreach$graphicsvg$GraphicSVG$ident,
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$black,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h));
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 3:
					var deg = shape.a;
					var sh = shape.b;
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = A2($MacCASOutreach$graphicsvg$GraphicSVG$rotateT, deg, trans),
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = sh;
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 4:
					var sx = shape.a;
					var sy = shape.b;
					var sh = shape.c;
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = A3($MacCASOutreach$graphicsvg$GraphicSVG$scaleT, sx, sy, trans),
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = sh;
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 5:
					var sx = shape.a;
					var sy = shape.b;
					var sh = shape.c;
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = A3($MacCASOutreach$graphicsvg$GraphicSVG$skewT, sx, sy, trans),
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = sh;
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 6:
					var tm = shape.a;
					var sh = shape.b;
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = A2($MacCASOutreach$graphicsvg$GraphicSVG$matrixMult, trans, tm),
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = sh;
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 13:
					var href = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$a,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$xlinkHref(href),
								$elm$svg$Svg$Attributes$target('_blank')
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 9:
					var region = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$defs,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$svg$Svg$mask,
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$id('m' + id)
											]),
										_List_fromArray(
											[
												A7(
												$MacCASOutreach$graphicsvg$GraphicSVG$createSVG,
												id + 'm',
												w,
												h,
												trans,
												msgWrapper,
												positionWrapper,
												$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Group(
													_List_fromArray(
														[
															$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Everything,
															A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, $MacCASOutreach$graphicsvg$GraphicSVG$black, region)
														])))
											]))
									])),
								A2(
								$elm$svg$Svg$g,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$mask('url(#m' + (id + ')'))
									]),
								_List_fromArray(
									[
										A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id + 'mm', w, h, trans, msgWrapper, positionWrapper, sh)
									]))
							]));
				case 10:
					var region = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$defs,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$svg$Svg$mask,
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$id('c' + id)
											]),
										_List_fromArray(
											[
												A7(
												$MacCASOutreach$graphicsvg$GraphicSVG$createSVG,
												id + 'c',
												w,
												h,
												trans,
												msgWrapper,
												positionWrapper,
												$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Group(
													_List_fromArray(
														[
															$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Notathing,
															A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, $MacCASOutreach$graphicsvg$GraphicSVG$white, region)
														])))
											]))
									])),
								A2(
								$elm$svg$Svg$g,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$mask('url(#c' + (id + ')'))
									]),
								_List_fromArray(
									[
										A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id + 'cc', w, h, trans, msgWrapper, positionWrapper, sh)
									]))
							]));
				case 14:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 15:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onTapAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 16:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$html$Html$Events$onMouseEnter(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 17:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onEnterAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 18:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$html$Html$Events$onMouseLeave(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 19:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onLeaveAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 20:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$html$Html$Events$onMouseDown(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 21:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onMouseDownAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 22:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$html$Html$Events$onMouseUp(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 23:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onMouseUpAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 24:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onMoveAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 25:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onTouchStart(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 26:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onTouchEnd(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 27:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onTouchStartAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 28:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onTouchStartAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 29:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onTouchMove(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 7:
					var shapes = shape.a;
					return A2(
						$elm$svg$Svg$g,
						_List_Nil,
						A2(
							$elm$core$List$indexedMap,
							function (n) {
								return A6(
									$MacCASOutreach$graphicsvg$GraphicSVG$createSVG,
									id + ('g' + $elm$core$String$fromInt(n)),
									w,
									h,
									trans,
									msgWrapper,
									positionWrapper);
							},
							shapes));
				case 8:
					var cmbndshp = shape.a;
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = trans,
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = cmbndshp;
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				default:
					var s = shape.a;
					var th = shape.b;
					var c = shape.c;
					return ((th <= 0) || (_Utils_cmp(s, 2 * th) < 0)) ? A2($elm$svg$Svg$g, _List_Nil, _List_Nil) : A7(
						$MacCASOutreach$graphicsvg$GraphicSVG$createSVG,
						id,
						w,
						h,
						trans,
						msgWrapper,
						positionWrapper,
						A4(
							$MacCASOutreach$graphicsvg$GraphicSVG$createGraph,
							_Utils_Tuple2(w, h),
							s,
							th,
							c));
			}
		}
	});
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $MacCASOutreach$graphicsvg$GraphicSVG$createCollage = F3(
	function (w, h, shapes) {
		return A2(
			$elm$svg$Svg$svg,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$width('100%'),
					$elm$svg$Svg$Attributes$height('100%'),
					$elm$svg$Svg$Attributes$style('position:absolute;top:0px;left:0px;'),
					$elm$svg$Svg$Attributes$viewBox(
					$elm$core$String$fromFloat((-w) / 2) + (' ' + ($elm$core$String$fromFloat((-h) / 2) + (' ' + ($elm$core$String$fromFloat(w) + (' ' + $elm$core$String$fromFloat(h))))))),
					$elm$svg$Svg$Attributes$id('render')
				]),
			A2(
				$elm$core$List$cons,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$cPath, w, h),
				_List_fromArray(
					[
						A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$clipPath('url(#cPath)')
							]),
						A2(
							$elm$core$List$indexedMap,
							function (n) {
								return A6(
									$MacCASOutreach$graphicsvg$GraphicSVG$createSVG,
									$elm$core$String$fromInt(n),
									w,
									h,
									$MacCASOutreach$graphicsvg$GraphicSVG$ident,
									$MacCASOutreach$graphicsvg$GraphicSVG$Graphics,
									$MacCASOutreach$graphicsvg$GraphicSVG$ReturnPosition);
							},
							shapes))
					])));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$hiddenAppView = F2(
	function (userView, _v0) {
		var userModel = _v0.a;
		var userViewEval = userView(userModel);
		var title = userViewEval.aV;
		var _v1 = userViewEval.aF;
		var w = _v1.a;
		var h = _v1.b;
		var shapes = _v1.c;
		return {
			aF: _List_fromArray(
				[
					A3($MacCASOutreach$graphicsvg$GraphicSVG$createCollage, w, h, shapes)
				]),
			aV: title
		};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$initHiddenModel = {A: 0, B: 0, N: 0, O: 0};
var $MacCASOutreach$graphicsvg$GraphicSVG$initialCmd = function (userCmd) {
	return $elm$core$Platform$Cmd$batch(
		_List_fromArray(
			[$MacCASOutreach$graphicsvg$GraphicSVG$getViewportSize, userCmd]));
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$map = _Platform_map;
var $elm$browser$Browser$Events$Window = 1;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {ao: pids, ay: subs};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (!node) {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {ad: event, ah: key};
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (!node) {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.ao,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.ah;
		var event = _v0.ad;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.ay);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		$elm$browser$Browser$Events$on,
		1,
		'resize',
		A2(
			$elm$json$Json$Decode$field,
			'target',
			A3(
				$elm$json$Json$Decode$map2,
				func,
				A2($elm$json$Json$Decode$field, 'innerWidth', $elm$json$Json$Decode$int),
				A2($elm$json$Json$Decode$field, 'innerHeight', $elm$json$Json$Decode$int))));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$subs = F2(
	function (userSubs, _v0) {
		var userModel = _v0.a;
		return $elm$core$Platform$Sub$batch(
			_Utils_ap(
				_List_fromArray(
					[
						$elm$browser$Browser$Events$onResize(
						F2(
							function (_v1, _v2) {
								return $MacCASOutreach$graphicsvg$GraphicSVG$WindowResize($elm$core$Maybe$Nothing);
							}))
					]),
				_List_fromArray(
					[
						A2(
						$elm$core$Platform$Sub$map,
						$MacCASOutreach$graphicsvg$GraphicSVG$Graphics,
						userSubs(userModel))
					])));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$app = function (input) {
	return $elm$browser$Browser$application(
		{
			aO: F3(
				function (flags, url, key) {
					var userInitCmd = A3(input.aO, flags, url, key).b;
					var userInit = A3(input.aO, flags, url, key).a;
					var userView = input.aX(userInit).aF;
					var _v0 = userView;
					var initW = _v0.a;
					var initH = _v0.b;
					return _Utils_Tuple2(
						_Utils_Tuple2(
							userInit,
							_Utils_update(
								$MacCASOutreach$graphicsvg$GraphicSVG$initHiddenModel,
								{A: initH, B: initW})),
						$MacCASOutreach$graphicsvg$GraphicSVG$initialCmd(
							A2($elm$core$Platform$Cmd$map, $MacCASOutreach$graphicsvg$GraphicSVG$Graphics, userInitCmd)));
				}),
			aQ: A2($elm$core$Basics$composeL, $MacCASOutreach$graphicsvg$GraphicSVG$Graphics, input.aQ),
			aR: A2($elm$core$Basics$composeL, $MacCASOutreach$graphicsvg$GraphicSVG$Graphics, input.aR),
			aU: $MacCASOutreach$graphicsvg$GraphicSVG$subs(input.aU),
			aW: A2($MacCASOutreach$graphicsvg$GraphicSVG$hiddenAppUpdate, input.aX, input.aW),
			aX: $MacCASOutreach$graphicsvg$GraphicSVG$hiddenAppView(input.aX)
		});
};
var $MacCASOutreach$graphicsvg$GraphicSVG$App$DownArrow = {$: 11};
var $MacCASOutreach$graphicsvg$GraphicSVG$App$Key = function (a) {
	return {$: 0, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$App$LeftArrow = {$: 8};
var $MacCASOutreach$graphicsvg$GraphicSVG$App$RightArrow = {$: 10};
var $MacCASOutreach$graphicsvg$GraphicSVG$App$UpArrow = {$: 9};
var $MacCASOutreach$graphicsvg$GraphicSVG$App$WentDown = 1;
var $MacCASOutreach$graphicsvg$GraphicSVG$App$WentUp = 0;
var $MacCASOutreach$graphicsvg$GraphicSVG$App$arrowChecker = F5(
	function (checker, up, down, left, right) {
		return _Utils_Tuple2(
			function () {
				var _v0 = _Utils_Tuple2(
					checker(left),
					checker(right));
				_v0$8:
				while (true) {
					switch (_v0.a) {
						case 1:
							switch (_v0.b) {
								case 3:
									var _v1 = _v0.a;
									var _v2 = _v0.b;
									return -1;
								case 2:
									var _v3 = _v0.a;
									var _v4 = _v0.b;
									return -1;
								default:
									break _v0$8;
							}
						case 0:
							switch (_v0.b) {
								case 3:
									var _v5 = _v0.a;
									var _v6 = _v0.b;
									return -1;
								case 2:
									var _v7 = _v0.a;
									var _v8 = _v0.b;
									return -1;
								default:
									break _v0$8;
							}
						case 3:
							switch (_v0.b) {
								case 1:
									var _v9 = _v0.a;
									var _v10 = _v0.b;
									return 1;
								case 0:
									var _v13 = _v0.a;
									var _v14 = _v0.b;
									return 1;
								default:
									break _v0$8;
							}
						default:
							switch (_v0.b) {
								case 1:
									var _v11 = _v0.a;
									var _v12 = _v0.b;
									return 1;
								case 0:
									var _v15 = _v0.a;
									var _v16 = _v0.b;
									return 1;
								default:
									break _v0$8;
							}
					}
				}
				return 0;
			}(),
			function () {
				var _v17 = _Utils_Tuple2(
					checker(down),
					checker(up));
				_v17$8:
				while (true) {
					switch (_v17.a) {
						case 1:
							switch (_v17.b) {
								case 3:
									var _v18 = _v17.a;
									var _v19 = _v17.b;
									return -1;
								case 2:
									var _v20 = _v17.a;
									var _v21 = _v17.b;
									return -1;
								default:
									break _v17$8;
							}
						case 0:
							switch (_v17.b) {
								case 3:
									var _v22 = _v17.a;
									var _v23 = _v17.b;
									return -1;
								case 2:
									var _v24 = _v17.a;
									var _v25 = _v17.b;
									return -1;
								default:
									break _v17$8;
							}
						case 3:
							switch (_v17.b) {
								case 1:
									var _v26 = _v17.a;
									var _v27 = _v17.b;
									return 1;
								case 0:
									var _v30 = _v17.a;
									var _v31 = _v17.b;
									return 1;
								default:
									break _v17$8;
							}
						default:
							switch (_v17.b) {
								case 1:
									var _v28 = _v17.a;
									var _v29 = _v17.b;
									return 1;
								case 0:
									var _v32 = _v17.a;
									var _v33 = _v17.b;
									return 1;
								default:
									break _v17$8;
							}
					}
				}
				return 0;
			}());
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$App$Down = 1;
var $MacCASOutreach$graphicsvg$GraphicSVG$App$JustDown = 0;
var $MacCASOutreach$graphicsvg$GraphicSVG$App$JustUp = 2;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$App$insertKeyDict = F3(
	function (dict, key, action) {
		var currState = A2($elm$core$Dict$get, key, dict);
		if (!currState.$) {
			if (!currState.a.b) {
				switch (currState.a.a) {
					case 0:
						var _v1 = currState.a;
						var _v2 = _v1.a;
						return A3(
							$elm$core$Dict$insert,
							key,
							function () {
								if (action === 1) {
									return _Utils_Tuple2(0, false);
								} else {
									return _Utils_Tuple2(0, true);
								}
							}(),
							dict);
					case 1:
						var _v4 = currState.a;
						var _v5 = _v4.a;
						return A3(
							$elm$core$Dict$insert,
							key,
							function () {
								if (action === 1) {
									return _Utils_Tuple2(1, false);
								} else {
									return _Utils_Tuple2(2, false);
								}
							}(),
							dict);
					case 3:
						var _v7 = currState.a;
						var _v8 = _v7.a;
						return A3(
							$elm$core$Dict$insert,
							key,
							function () {
								if (action === 1) {
									return _Utils_Tuple2(0, false);
								} else {
									return _Utils_Tuple2(2, false);
								}
							}(),
							dict);
					default:
						var _v10 = currState.a;
						var _v11 = _v10.a;
						return A3(
							$elm$core$Dict$insert,
							key,
							function () {
								if (action === 1) {
									return _Utils_Tuple2(2, true);
								} else {
									return _Utils_Tuple2(2, false);
								}
							}(),
							dict);
				}
			} else {
				var _v13 = currState.a;
				var state = _v13.a;
				return A3(
					$elm$core$Dict$insert,
					key,
					function () {
						if (action === 1) {
							return _Utils_Tuple2(state, true);
						} else {
							return _Utils_Tuple2(state, true);
						}
					}(),
					dict);
			}
		} else {
			return A3(
				$elm$core$Dict$insert,
				key,
				function () {
					if (action === 1) {
						return _Utils_Tuple2(0, false);
					} else {
						return _Utils_Tuple2(2, false);
					}
				}(),
				dict);
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$App$Up = 3;
var $elm$core$Char$toUpper = _Char_toUpper;
var $MacCASOutreach$graphicsvg$GraphicSVG$App$keyCheckerFunction = F2(
	function (dict, key) {
		var kc = function () {
			switch (key.$) {
				case 0:
					var str = key.a;
					return $elm$core$Char$toCode(
						$elm$core$Char$toUpper(
							function () {
								var _v10 = $elm$core$String$uncons(str);
								if (!_v10.$) {
									var _v11 = _v10.a;
									var a = _v11.a;
									return a;
								} else {
									return 'z';
								}
							}()));
				case 1:
					return 8;
				case 2:
					return 9;
				case 3:
					return 13;
				case 4:
					return 16;
				case 5:
					return 17;
				case 6:
					return 18;
				case 7:
					return 20;
				case 13:
					return 32;
				case 8:
					return 37;
				case 9:
					return 38;
				case 10:
					return 39;
				case 11:
					return 40;
				default:
					return 46;
			}
		}();
		var state = A2($elm$core$Dict$get, kc, dict);
		if (!state.$) {
			switch (state.a.a) {
				case 0:
					var _v1 = state.a;
					var _v2 = _v1.a;
					return 0;
				case 1:
					var _v3 = state.a;
					var _v4 = _v3.a;
					return 1;
				case 2:
					var _v5 = state.a;
					var _v6 = _v5.a;
					return 2;
				default:
					var _v7 = state.a;
					var _v8 = _v7.a;
					return 3;
			}
		} else {
			return 3;
		}
	});
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$App$filterHelper = F2(
	function (key, action) {
		if (action.a === 3) {
			var _v1 = action.a;
			return false;
		} else {
			return true;
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$App$maintainHelper = F2(
	function (key, action) {
		if (!action.b) {
			switch (action.a) {
				case 2:
					var _v1 = action.a;
					return _Utils_Tuple2(3, false);
				case 3:
					var _v3 = action.a;
					return _Utils_Tuple2(3, false);
				case 0:
					var _v5 = action.a;
					return _Utils_Tuple2(1, false);
				default:
					var _v7 = action.a;
					return _Utils_Tuple2(1, false);
			}
		} else {
			switch (action.a) {
				case 2:
					var _v2 = action.a;
					return _Utils_Tuple2(0, false);
				case 3:
					var _v4 = action.a;
					return _Utils_Tuple2(3, false);
				case 0:
					var _v6 = action.a;
					return _Utils_Tuple2(2, false);
				default:
					var _v8 = action.a;
					return _Utils_Tuple2(1, false);
			}
		}
	});
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$App$maintainKeyDict = function (dict) {
	return A2(
		$elm$core$Dict$filter,
		$MacCASOutreach$graphicsvg$GraphicSVG$App$filterHelper,
		A2($elm$core$Dict$map, $MacCASOutreach$graphicsvg$GraphicSVG$App$maintainHelper, dict));
};
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0;
	return millis;
};
var $MacCASOutreach$graphicsvg$GraphicSVG$App$subtractTimeSeconds = F2(
	function (t1, t0) {
		return ($elm$time$Time$posixToMillis(t1) - $elm$time$Time$posixToMillis(t0)) / 1000;
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$App$hiddenTickUpdate = F3(
	function (userUpdate, msg, _v0) {
		var userModel = _v0.a;
		var hiddenModel = _v0.b;
		var updateTick = hiddenModel.F;
		switch (msg.$) {
			case 0:
				var userMsg = msg.a;
				var _v2 = A2(userUpdate, userMsg, userModel);
				var newUserModel = _v2.a;
				var newUserCmds = _v2.b;
				return _Utils_Tuple2(
					_Utils_Tuple2(newUserModel, hiddenModel),
					A2($elm$core$Platform$Cmd$map, $MacCASOutreach$graphicsvg$GraphicSVG$App$UserMsg, newUserCmds));
			case 1:
				var t = msg.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						userModel,
						_Utils_update(
							hiddenModel,
							{D: t})),
					$elm$core$Platform$Cmd$none);
			case 2:
				var t = msg.a;
				var timeInSeconds = A2($MacCASOutreach$graphicsvg$GraphicSVG$App$subtractTimeSeconds, t, hiddenModel.D);
				var keyChecker = $MacCASOutreach$graphicsvg$GraphicSVG$App$keyCheckerFunction(hiddenModel.g);
				var wasd = A5(
					$MacCASOutreach$graphicsvg$GraphicSVG$App$arrowChecker,
					keyChecker,
					$MacCASOutreach$graphicsvg$GraphicSVG$App$Key('w'),
					$MacCASOutreach$graphicsvg$GraphicSVG$App$Key('s'),
					$MacCASOutreach$graphicsvg$GraphicSVG$App$Key('a'),
					$MacCASOutreach$graphicsvg$GraphicSVG$App$Key('d'));
				var arrowKeys = A5($MacCASOutreach$graphicsvg$GraphicSVG$App$arrowChecker, keyChecker, $MacCASOutreach$graphicsvg$GraphicSVG$App$UpArrow, $MacCASOutreach$graphicsvg$GraphicSVG$App$DownArrow, $MacCASOutreach$graphicsvg$GraphicSVG$App$LeftArrow, $MacCASOutreach$graphicsvg$GraphicSVG$App$RightArrow);
				var _v3 = A2(
					userUpdate,
					A2(
						hiddenModel.F,
						timeInSeconds,
						_Utils_Tuple3(keyChecker, arrowKeys, wasd)),
					userModel);
				var newUserModel = _v3.a;
				var newUserCmds = _v3.b;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						newUserModel,
						_Utils_update(
							hiddenModel,
							{
								g: $MacCASOutreach$graphicsvg$GraphicSVG$App$maintainKeyDict(hiddenModel.g)
							})),
					A2($elm$core$Platform$Cmd$map, $MacCASOutreach$graphicsvg$GraphicSVG$App$UserMsg, newUserCmds));
			case 3:
				var keyCode = msg.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						userModel,
						_Utils_update(
							hiddenModel,
							{
								g: A3($MacCASOutreach$graphicsvg$GraphicSVG$App$insertKeyDict, hiddenModel.g, keyCode, 1)
							})),
					$elm$core$Platform$Cmd$none);
			case 4:
				var keyCode = msg.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						userModel,
						_Utils_update(
							hiddenModel,
							{
								g: A3($MacCASOutreach$graphicsvg$GraphicSVG$App$insertKeyDict, hiddenModel.g, keyCode, 0)
							})),
					$elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_Tuple2(userModel, hiddenModel),
					$elm$core$Platform$Cmd$none);
		}
	});
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $MacCASOutreach$graphicsvg$GraphicSVG$App$initHiddenModel = F2(
	function (tick, navKey) {
		return {
			D: $elm$time$Time$millisToPosix(0),
			g: $elm$core$Dict$empty,
			al: navKey,
			F: tick
		};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Collage = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$map = F2(
	function (f, sh) {
		switch (sh.$) {
			case 0:
				var fillClr = sh.a;
				var lt = sh.b;
				var stencil = sh.c;
				return A3($MacCASOutreach$graphicsvg$GraphicSVG$Secret$Inked, fillClr, lt, stencil);
			case 1:
				var w = sh.a;
				var h = sh.b;
				var htm = sh.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$ForeignObject,
					w,
					h,
					A2($elm$html$Html$map, f, htm));
			case 2:
				var v = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Move,
					v,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 3:
				var deg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Rotate,
					deg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 4:
				var sx = sh.a;
				var sy = sh.b;
				var shape = sh.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Scale,
					sx,
					sy,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 5:
				var skx = sh.a;
				var sky = sh.b;
				var shape = sh.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Skew,
					skx,
					sky,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 6:
				var tm = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Transformed,
					tm,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 13:
				var href = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Link,
					href,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 9:
				var sh1 = sh.a;
				var sh2 = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$AlphaMask,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, sh1),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, sh2));
			case 10:
				var sh1 = sh.a;
				var sh2 = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Clip,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, sh1),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, sh2));
			case 11:
				return $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Everything;
			case 12:
				return $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Notathing;
			case 14:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Tap,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 15:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$TapAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 16:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$EnterShape,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 17:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$EnterAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 18:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Exit,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 19:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$ExitAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 20:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$MouseDown,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 21:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$MouseDownAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 22:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$MouseUp,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 23:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$MouseUpAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 24:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$MoveOverAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 25:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchStart,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 26:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchEnd,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 27:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchStartAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 28:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchEndAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 29:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Secret$TouchMoveAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 7:
				var shapes = sh.a;
				return $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Group(
					A2(
						$elm$core$List$map,
						$MacCASOutreach$graphicsvg$GraphicSVG$map(f),
						shapes));
			case 8:
				var cmbndshp = sh.a;
				return $MacCASOutreach$graphicsvg$GraphicSVG$Secret$GroupOutline(
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, cmbndshp));
			default:
				var s = sh.a;
				var th = sh.b;
				var c = sh.c;
				return A3($MacCASOutreach$graphicsvg$GraphicSVG$Secret$GraphPaper, s, th, c);
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$mapCollage = F2(
	function (f, _v0) {
		var w = _v0.a;
		var h = _v0.b;
		var shapes = _v0.c;
		return A3(
			$MacCASOutreach$graphicsvg$GraphicSVG$Collage,
			w,
			h,
			A2(
				$elm$core$List$map,
				$MacCASOutreach$graphicsvg$GraphicSVG$map(f),
				shapes));
	});
var $elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $MacCASOutreach$graphicsvg$GraphicSVG$App$KeyDown = function (a) {
	return {$: 3, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$App$KeyUp = function (a) {
	return {$: 4, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$App$TickTime = function (a) {
	return {$: 2, a: a};
};
var $elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 0, a: a};
};
var $elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {U: oldTime, av: request, ay: subs};
	});
var $elm$browser$Browser$AnimationManager$init = $elm$core$Task$succeed(
	A3($elm$browser$Browser$AnimationManager$State, _List_Nil, $elm$core$Maybe$Nothing, 0));
var $elm$browser$Browser$AnimationManager$now = _Browser_now(0);
var $elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(0);
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _v0) {
		var request = _v0.av;
		var oldTime = _v0.U;
		var _v1 = _Utils_Tuple2(request, subs);
		if (_v1.a.$ === 1) {
			if (!_v1.b.b) {
				var _v2 = _v1.a;
				return $elm$browser$Browser$AnimationManager$init;
			} else {
				var _v4 = _v1.a;
				return A2(
					$elm$core$Task$andThen,
					function (pid) {
						return A2(
							$elm$core$Task$andThen,
							function (time) {
								return $elm$core$Task$succeed(
									A3(
										$elm$browser$Browser$AnimationManager$State,
										subs,
										$elm$core$Maybe$Just(pid),
										time));
							},
							$elm$browser$Browser$AnimationManager$now);
					},
					$elm$core$Process$spawn(
						A2(
							$elm$core$Task$andThen,
							$elm$core$Platform$sendToSelf(router),
							$elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_v1.b.b) {
				var pid = _v1.a.a;
				return A2(
					$elm$core$Task$andThen,
					function (_v3) {
						return $elm$browser$Browser$AnimationManager$init;
					},
					$elm$core$Process$kill(pid));
			} else {
				return $elm$core$Task$succeed(
					A3($elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var $elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _v0) {
		var subs = _v0.ay;
		var oldTime = _v0.U;
		var send = function (sub) {
			if (!sub.$) {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(
						$elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			$elm$core$Task$andThen,
			function (pid) {
				return A2(
					$elm$core$Task$andThen,
					function (_v1) {
						return $elm$core$Task$succeed(
							A3(
								$elm$browser$Browser$AnimationManager$State,
								subs,
								$elm$core$Maybe$Just(pid),
								newTime));
					},
					$elm$core$Task$sequence(
						A2($elm$core$List$map, send, subs)));
			},
			$elm$core$Process$spawn(
				A2(
					$elm$core$Task$andThen,
					$elm$core$Platform$sendToSelf(router),
					$elm$browser$Browser$AnimationManager$rAF)));
	});
var $elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (!sub.$) {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Time(
				A2($elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Delta(
				A2($elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager($elm$browser$Browser$AnimationManager$init, $elm$browser$Browser$AnimationManager$onEffects, $elm$browser$Browser$AnimationManager$onSelfMsg, 0, $elm$browser$Browser$AnimationManager$subMap);
var $elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var $elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return $elm$browser$Browser$AnimationManager$subscription(
		$elm$browser$Browser$AnimationManager$Time(tagger));
};
var $elm$browser$Browser$Events$onAnimationFrame = $elm$browser$Browser$AnimationManager$onAnimationFrame;
var $elm$browser$Browser$Events$Document = 0;
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, 0, 'keydown');
var $elm$browser$Browser$Events$onKeyUp = A2($elm$browser$Browser$Events$on, 0, 'keyup');
var $MacCASOutreach$graphicsvg$GraphicSVG$App$subs = _List_fromArray(
	[
		$elm$browser$Browser$Events$onKeyUp(
		A2(
			$elm$json$Json$Decode$map,
			$MacCASOutreach$graphicsvg$GraphicSVG$App$KeyUp,
			A2($elm$json$Json$Decode$field, 'keyCode', $elm$json$Json$Decode$int))),
		$elm$browser$Browser$Events$onKeyDown(
		A2(
			$elm$json$Json$Decode$map,
			$MacCASOutreach$graphicsvg$GraphicSVG$App$KeyDown,
			A2($elm$json$Json$Decode$field, 'keyCode', $elm$json$Json$Decode$int))),
		$elm$browser$Browser$Events$onAnimationFrame($MacCASOutreach$graphicsvg$GraphicSVG$App$TickTime)
	]);
var $MacCASOutreach$graphicsvg$GraphicSVG$App$appWithTick = F2(
	function (tickMsg, userApp) {
		var userView = userApp.aX;
		var userUrlReq = userApp.aR;
		var userUrlChange = userApp.aQ;
		var userUpdate = userApp.aW;
		var userSubs = userApp.aU;
		var userInit = userApp.aO;
		return $MacCASOutreach$graphicsvg$GraphicSVG$app(
			{
				aO: F3(
					function (flags, url, navKey) {
						var userInitModel = A3(userInit, flags, url, navKey).a;
						var userInitCmds = A3(userInit, flags, url, navKey).b;
						return _Utils_Tuple2(
							_Utils_Tuple2(
								userInitModel,
								A2($MacCASOutreach$graphicsvg$GraphicSVG$App$initHiddenModel, tickMsg, navKey)),
							$elm$core$Platform$Cmd$batch(
								_List_fromArray(
									[
										A2($elm$core$Task$perform, $MacCASOutreach$graphicsvg$GraphicSVG$App$InitTime, $elm$time$Time$now),
										A2($elm$core$Platform$Cmd$map, $MacCASOutreach$graphicsvg$GraphicSVG$App$UserMsg, userInitCmds)
									])));
					}),
				aQ: A2($elm$core$Basics$composeL, $MacCASOutreach$graphicsvg$GraphicSVG$App$UserMsg, userUrlChange),
				aR: A2($elm$core$Basics$composeL, $MacCASOutreach$graphicsvg$GraphicSVG$App$UserMsg, userUrlReq),
				aU: function (_v0) {
					var userModel = _v0.a;
					return $elm$core$Platform$Sub$batch(
						A2(
							$elm$core$List$cons,
							A2(
								$elm$core$Platform$Sub$map,
								$MacCASOutreach$graphicsvg$GraphicSVG$App$UserMsg,
								userSubs(userModel)),
							$MacCASOutreach$graphicsvg$GraphicSVG$App$subs));
				},
				aW: $MacCASOutreach$graphicsvg$GraphicSVG$App$hiddenTickUpdate(userUpdate),
				aX: function (_v1) {
					var userModel = _v1.a;
					var userViewE = userView(userModel);
					return {
						aF: A2($MacCASOutreach$graphicsvg$GraphicSVG$mapCollage, $MacCASOutreach$graphicsvg$GraphicSVG$App$UserMsg, userViewE.aF),
						aV: userViewE.aV
					};
				}
			});
	});
var $author$project$Main$ButtercupWay = 1;
var $author$project$Main$DaffodilWay = 2;
var $author$project$Main$SZip = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $author$project$Main$TrainStation = 0;
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Main$sendSpeech = _Platform_outgoingPort('sendSpeech', $elm$json$Json$Encode$string);
var $author$project$Main$init = F3(
	function (_v0, _v1, _v2) {
		return _Utils_Tuple2(
			{
				a: A3(
					$author$project$Main$SZip,
					_List_Nil,
					1,
					_List_fromArray(
						[2])),
				b: 0,
				o: 0,
				c: 0
			},
			$author$project$Main$sendSpeech('You are at TrainStation. You may proceed to ButtercupWay or DaffodilWay. ButtercupWay is currently selected.'));
	});
var $author$project$Main$KeyDown = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$subscriptions = function (_v0) {
	return $elm$browser$Browser$Events$onKeyDown(
		A2(
			$elm$json$Json$Decode$map,
			$author$project$Main$KeyDown,
			A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string)));
};
var $author$project$Main$BullrushWay = 5;
var $author$project$Main$FireweedWay = 4;
var $author$project$Main$LillyPond = 6;
var $author$project$Main$MountainPass = 3;
var $author$project$Main$stateToStr = function (state) {
	switch (state) {
		case 0:
			return 'TrainStation';
		case 1:
			return 'ButtercupWay';
		case 2:
			return 'DaffodilWay';
		case 3:
			return 'MountainPass';
		case 4:
			return 'FireweedWay';
		case 5:
			return 'BullrushWay';
		default:
			return 'LillyPond';
	}
};
var $author$project$Main$focusChangeStr = function (zip) {
	var focused = zip.b;
	return 'You have selected ' + ($author$project$Main$stateToStr(focused) + '. Press enter to move to this state.');
};
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$Main$strToState = function (str) {
	switch (str) {
		case 'TrainStation':
			return $elm$core$Maybe$Just(0);
		case 'ButtercupWay':
			return $elm$core$Maybe$Just(1);
		case 'DaffodilWay':
			return $elm$core$Maybe$Just(2);
		case 'MountainPass':
			return $elm$core$Maybe$Just(3);
		case 'FireweedWay':
			return $elm$core$Maybe$Just(4);
		case 'BullrushWay':
			return $elm$core$Maybe$Just(5);
		case 'LillyPond':
			return $elm$core$Maybe$Just(6);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Main$nextStates = function (states) {
	return A2(
		$elm$core$List$map,
		function (s) {
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				$author$project$Main$strToState(s));
		},
		states);
};
var $author$project$Main$abbrToStateStr = function (abbr) {
	switch (abbr) {
		case 'TS':
			return 'TrainStation';
		case 'BCW':
			return 'ButtercupWay';
		case 'DW':
			return 'DaffodilWay';
		case 'MP':
			return 'MountainPass';
		case 'FW':
			return 'FireweedWay';
		case 'BW':
			return 'BullrushWay';
		case 'LP':
			return 'LillyPond';
		default:
			return '';
	}
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $author$project$Main$stateToAbbr = function (state) {
	switch (state) {
		case 0:
			return 'TS';
		case 1:
			return 'BCW';
		case 2:
			return 'DW';
		case 3:
			return 'MP';
		case 4:
			return 'FW';
		case 5:
			return 'BW';
		default:
			return 'LP';
	}
};
var $author$project$Main$nextStatesStr = function (state) {
	var msgs = _List_fromArray(
		['TS2BCW', 'BCW2TS', 'TS2DW', 'DW2TS', 'DW2BCW', 'BCW2DW', 'BCW2MP', 'MP2DW', 'MP2FW', 'FW2MP', 'FW2BW', 'BW2FW', 'MP2BW', 'LP2BW', 'BW2LP']);
	var abbr = $author$project$Main$stateToAbbr(state);
	return A2(
		$elm$core$List$map,
		$author$project$Main$abbrToStateStr,
		A2(
			$elm$core$List$map,
			A2($elm$core$String$replace, abbr + '2', ''),
			A2(
				$elm$core$List$filter,
				$elm$core$String$startsWith(abbr),
				msgs)));
};
var $author$project$Main$sNext = function (_v0) {
	var before = _v0.a;
	var focused = _v0.b;
	var after = _v0.c;
	if (!after.b) {
		return A3($author$project$Main$SZip, before, focused, after);
	} else {
		var a = after.a;
		var rest = after.b;
		return A3(
			$author$project$Main$SZip,
			A2($elm$core$List$cons, focused, before),
			a,
			rest);
	}
};
var $author$project$Main$sPrev = function (_v0) {
	var before = _v0.a;
	var focused = _v0.b;
	var after = _v0.c;
	if (!before.b) {
		return A3($author$project$Main$SZip, before, focused, after);
	} else {
		var a = before.a;
		var rest = before.b;
		return A3(
			$author$project$Main$SZip,
			rest,
			a,
			A2($elm$core$List$cons, focused, after));
	}
};
var $author$project$Main$formatListWithOr = function (list) {
	var _v0 = $elm$core$List$reverse(list);
	if (!_v0.b) {
		return '';
	} else {
		if (!_v0.b.b) {
			var last = _v0.a;
			return last;
		} else {
			var last = _v0.a;
			var rest = _v0.b;
			return A2(
				$elm$core$String$join,
				', ',
				$elm$core$List$reverse(rest)) + (', or ' + last);
		}
	}
};
var $author$project$Main$stateToSpeechStr = F2(
	function (state, zip) {
		var nextStr = $author$project$Main$formatListWithOr(
			$author$project$Main$nextStatesStr(state));
		var current = $author$project$Main$stateToStr(state);
		var focused = zip.b;
		return 'You are at ' + (current + ('. You may proceed to ' + (nextStr + ('. ' + ($author$project$Main$stateToStr(focused) + ' is currently selected. Use the left and right arrow keys to change selection.')))));
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var t = msg.a;
				if (model.c >= 15) {
					var _v1 = model.b;
					switch (_v1) {
						case 0:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{o: t, c: 0}),
								$author$project$Main$sendSpeech(
									A2($author$project$Main$stateToSpeechStr, 0, model.a)));
						case 1:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{o: t, c: 0}),
								$author$project$Main$sendSpeech(
									A2($author$project$Main$stateToSpeechStr, 1, model.a)));
						case 2:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{o: t, c: 0}),
								$author$project$Main$sendSpeech(
									A2($author$project$Main$stateToSpeechStr, 2, model.a)));
						case 3:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{o: t, c: 0}),
								$author$project$Main$sendSpeech(
									A2($author$project$Main$stateToSpeechStr, 3, model.a)));
						case 4:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{o: t, c: 0}),
								$author$project$Main$sendSpeech(
									A2($author$project$Main$stateToSpeechStr, 4, model.a)));
						case 5:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{o: t, c: 0}),
								$author$project$Main$sendSpeech(
									A2($author$project$Main$stateToSpeechStr, 5, model.a)));
						default:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{o: t, c: 0}),
								$author$project$Main$sendSpeech(
									A2($author$project$Main$stateToSpeechStr, 6, model.a)));
					}
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{o: t, c: model.c + 0.01}),
						$elm$core$Platform$Cmd$none);
				}
			case 2:
				var _v2 = model.b;
				if (!_v2) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(1));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 1,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 1, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 1, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 1, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 3:
				var _v4 = model.b;
				if (_v4 === 1) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(0));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 0,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 0, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 0, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 0, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 4:
				var _v6 = model.b;
				if (!_v6) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(2));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 2,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 2, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 2, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 2, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 5:
				var _v8 = model.b;
				if (_v8 === 2) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(0));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 0,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 0, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 0, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 0, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 6:
				var _v10 = model.b;
				if (_v10 === 2) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{b: 1, c: 0}),
						$author$project$Main$sendSpeech(
							A2($author$project$Main$stateToSpeechStr, 1, model.a)));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 7:
				var _v11 = model.b;
				if (_v11 === 1) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(2));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 2,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 2, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 2, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 2, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 8:
				var _v13 = model.b;
				if (_v13 === 1) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(3));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 3,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 3, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 3, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 3, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 9:
				var _v15 = model.b;
				if (_v15 === 3) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(2));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 2,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 2, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 2, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 2, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 10:
				var _v17 = model.b;
				if (_v17 === 3) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(4));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 4,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 4, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 4, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 4, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 11:
				var _v19 = model.b;
				if (_v19 === 4) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(3));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 3,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 3, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 3, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 3, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 12:
				var _v21 = model.b;
				if (_v21 === 4) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(5));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 5,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 5, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 5, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 5, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 13:
				var _v23 = model.b;
				if (_v23 === 5) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(4));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 4,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 4, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 4, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 4, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 14:
				var _v25 = model.b;
				if (_v25 === 3) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(5));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 5,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 5, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 5, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 5, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 15:
				var _v27 = model.b;
				if (_v27 === 6) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(5));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 5,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 5, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 5, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 5, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 16:
				var _v29 = model.b;
				if (_v29 === 5) {
					var nextStatesList = $author$project$Main$nextStates(
						$author$project$Main$nextStatesStr(6));
					if (nextStatesList.b) {
						var a = nextStatesList.a;
						var rest = nextStatesList.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									a: A3($author$project$Main$SZip, _List_Nil, a, rest),
									b: 6,
									c: 0
								}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 6, model.a)));
					} else {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{b: 6, c: 0}),
							$author$project$Main$sendSpeech(
								A2($author$project$Main$stateToSpeechStr, 6, model.a)));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 17:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			default:
				var code = msg.a;
				switch (code) {
					case 'ArrowRight':
						var updatedZipper = $author$project$Main$sNext(model.a);
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{a: updatedZipper, c: 0}),
							$author$project$Main$sendSpeech(
								$author$project$Main$focusChangeStr(updatedZipper)));
					case 'ArrowLeft':
						var updatedZipper = $author$project$Main$sPrev(model.a);
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{a: updatedZipper, c: 0}),
							$author$project$Main$sendSpeech(
								$author$project$Main$focusChangeStr(updatedZipper)));
					case 'Enter':
						var _v32 = model.a;
						var focused = _v32.b;
						var _v33 = $author$project$Main$nextStates(
							$author$project$Main$nextStatesStr(focused));
						if (_v33.b) {
							var a = _v33.a;
							var rest = _v33.b;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										a: A3($author$project$Main$SZip, _List_Nil, a, rest),
										b: focused,
										c: 0
									}),
								$author$project$Main$sendSpeech(
									A2(
										$author$project$Main$stateToSpeechStr,
										focused,
										A3($author$project$Main$SZip, _List_Nil, a, rest))));
						} else {
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						}
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$collage = F3(
	function (w, h, shapes) {
		return A3($MacCASOutreach$graphicsvg$GraphicSVG$Collage, w, h, shapes);
	});
var $author$project$Main$BCW2DW = {$: 7};
var $author$project$Main$BCW2MP = {$: 8};
var $author$project$Main$BCW2TS = {$: 3};
var $author$project$Main$BW2FW = {$: 13};
var $author$project$Main$BW2LP = {$: 16};
var $author$project$Main$DW2BCW = {$: 6};
var $author$project$Main$DW2TS = {$: 5};
var $author$project$Main$FW2BW = {$: 12};
var $author$project$Main$FW2MP = {$: 11};
var $author$project$Main$LP2BW = {$: 15};
var $author$project$Main$MP2BW = {$: 14};
var $author$project$Main$MP2DW = {$: 9};
var $author$project$Main$MP2FW = {$: 10};
var $author$project$Main$TS2BCW = {$: 2};
var $author$project$Main$TS2DW = {$: 4};
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$AlignCentred = 1;
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Face = F8(
	function (a, b, c, d, e, f, g, h) {
		return {$: 0, a: a, b: b, c: c, d: d, e: e, f: f, g: g, h: h};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Text = F2(
	function (a, b) {
		return {$: 7, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$centered = function (stencil) {
	if (stencil.$ === 7) {
		var _v1 = stencil.a;
		var si = _v1.a;
		var bo = _v1.b;
		var i = _v1.c;
		var u = _v1.d;
		var s = _v1.e;
		var sel = _v1.f;
		var f = _v1.g;
		var c = _v1.h;
		var str = stencil.b;
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Text,
			A8($MacCASOutreach$graphicsvg$GraphicSVG$Secret$Face, si, bo, i, u, s, sel, f, 1),
			str);
	} else {
		var a = stencil;
		return a;
	}
};
var $MacCASOutreach$graphicsvg$GraphicSVG$green = A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 115, 210, 22, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$notifyTap = F2(
	function (msg, shape) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$Secret$Tap, msg, shape);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$pink = A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 105, 180, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$RoundRect = F3(
	function (a, b, c) {
		return {$: 2, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$roundedRect = F3(
	function (w, h, r) {
		return A3($MacCASOutreach$graphicsvg$GraphicSVG$Secret$RoundRect, w, h, r);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$size = F2(
	function (sze, stencil) {
		if (stencil.$ === 7) {
			var _v1 = stencil.a;
			var si = _v1.a;
			var bo = _v1.b;
			var i = _v1.c;
			var u = _v1.d;
			var s = _v1.e;
			var sel = _v1.f;
			var f = _v1.g;
			var c = _v1.h;
			var str = stencil.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Text,
				A8($MacCASOutreach$graphicsvg$GraphicSVG$Secret$Face, sze, bo, i, u, s, sel, f, c),
				str);
		} else {
			var a = stencil;
			return a;
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$AlignLeft = 0;
var $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Serif = {$: 0};
var $MacCASOutreach$graphicsvg$GraphicSVG$text = function (str) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$Secret$Text,
		A8($MacCASOutreach$graphicsvg$GraphicSVG$Secret$Face, 12, false, false, false, false, false, $MacCASOutreach$graphicsvg$GraphicSVG$Secret$Serif, 0),
		str);
};
var $author$project$Main$myShapes = function (model) {
	var highlightIfFocused = F2(
		function (focusedState, thisState) {
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				_Utils_eq(focusedState, thisState) ? $MacCASOutreach$graphicsvg$GraphicSVG$pink : $MacCASOutreach$graphicsvg$GraphicSVG$green,
				A3($MacCASOutreach$graphicsvg$GraphicSVG$roundedRect, 40, 20, 5));
		});
	var currentFocus = function () {
		var _v1 = model.a;
		var f = _v1.b;
		return f;
	}();
	var _v0 = model.b;
	switch (_v0) {
		case 0:
			return _List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					$MacCASOutreach$graphicsvg$GraphicSVG$centered(
						$MacCASOutreach$graphicsvg$GraphicSVG$text('TrainStation'))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$TS2BCW,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-25, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 1),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('TS2BCW')))))
								])))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$TS2DW,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(25, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 2),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('TS2DW')))))
								]))))
				]);
		case 1:
			return _List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					$MacCASOutreach$graphicsvg$GraphicSVG$centered(
						$MacCASOutreach$graphicsvg$GraphicSVG$text('ButtercupWay'))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$BCW2TS,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-50, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 0),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('BCW2TS')))))
								])))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$BCW2DW,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(0, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 2),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('BCW2DW')))))
								])))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$BCW2MP,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(50, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 3),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('BCW2MP')))))
								]))))
				]);
		case 2:
			return _List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					$MacCASOutreach$graphicsvg$GraphicSVG$centered(
						$MacCASOutreach$graphicsvg$GraphicSVG$text('DaffodilWay'))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$DW2TS,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-25, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 0),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('DW2TS')))))
								])))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$DW2BCW,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(25, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 1),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('DW2BCW')))))
								]))))
				]);
		case 3:
			return _List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					$MacCASOutreach$graphicsvg$GraphicSVG$centered(
						$MacCASOutreach$graphicsvg$GraphicSVG$text('MountainPass'))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$MP2DW,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-50, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 2),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('MP2DW')))))
								])))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$MP2FW,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(0, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 4),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('MP2FW')))))
								])))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$MP2BW,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(50, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 5),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('MP2BW')))))
								]))))
				]);
		case 4:
			return _List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					$MacCASOutreach$graphicsvg$GraphicSVG$centered(
						$MacCASOutreach$graphicsvg$GraphicSVG$text('FireweedWay'))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$FW2MP,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-25, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 3),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('FW2MP')))))
								])))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$FW2BW,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(25, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 5),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('FW2BW')))))
								]))))
				]);
		case 5:
			return _List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					$MacCASOutreach$graphicsvg$GraphicSVG$centered(
						$MacCASOutreach$graphicsvg$GraphicSVG$text('BullrushWay'))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$BW2FW,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-25, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 4),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('BW2FW')))))
								])))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$BW2LP,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(25, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 6),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('BW2LP')))))
								]))))
				]);
		default:
			return _List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					$MacCASOutreach$graphicsvg$GraphicSVG$centered(
						$MacCASOutreach$graphicsvg$GraphicSVG$text('LillyPond'))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$Main$LP2BW,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(0, -25),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							_List_fromArray(
								[
									A2(highlightIfFocused, currentFocus, 5),
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -3),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$black,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											8,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text('LP2BW')))))
								]))))
				]);
	}
};
var $author$project$Main$view = function (model) {
	return {
		aF: A3(
			$MacCASOutreach$graphicsvg$GraphicSVG$collage,
			500,
			500,
			$author$project$Main$myShapes(model)),
		aV: 'My App'
	};
};
var $author$project$Main$main = A2(
	$MacCASOutreach$graphicsvg$GraphicSVG$App$appWithTick,
	$author$project$Main$Tick,
	{
		aO: $author$project$Main$init,
		aQ: function (_v0) {
			return $author$project$Main$NoOp;
		},
		aR: function (_v1) {
			return $author$project$Main$NoOp;
		},
		aU: $author$project$Main$subscriptions,
		aW: $author$project$Main$update,
		aX: $author$project$Main$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));