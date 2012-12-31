# JavaScript in a week

## Day 1 defining a subset of JavaScript

The implementation should execute the following JavaScript code.

```js
var acc = 'dummy';
var f = function() {
	var acc = [];
	return function(x) {
		return x ? acc.push(x)&&f(x - 1) : acc;
	};
  console.log("this won't show up!");
}();

String.prototype.ujihisa = function() {
	var x = 10;
	return this + f(x);
}

console.log("hello".ujihisa());
console.log(acc == 'dummy');
console.log({'a': 2}.a == 2);
```

    hello10,9,8,7,6,5,4,3,2,1
    true

That code covers the following important aspects.

* statements and expressions
* literals (String, Number, Function, Array and Object)
* function call, method call, operators, and index/key access of Object
* implicit convertion (String + Number)
* lexical scope (see `acc` var)
* method addition by prototype
* `return` exits from the current function

The implementation consists of

* parser (JS code to internal representation in S-expression)
* runtime

I'll work on runtime first in Clojure, then work on parser in Haskell with parsec.

### parsing

I don't implement the parser for now but I simply parse the sample code by hand. the first 2 statements will be like the below.

before:

```js
var acc = 'dummy';
var f = function() {
	var acc = [];
	return function(x) {
		return x ? acc.push(x)&&f(x - 1) : acc;
	};
}();
```

after:

```clojure
'[(var 'acc "dummy")
  (var 'f
       (call
         (function []
                   [[(var 'acc (array))
                     (return
                       (function ['x]
                                 [[(return (if 'x
                                             (and (mcall 'push 'acc ['x])
                                                  (call 'f [(minus 'x 1)]))
                                             'acc))]]))]])
         []))]
```
