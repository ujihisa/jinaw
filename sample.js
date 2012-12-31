var acc = 'dummy';
var f = function() {
	var acc = [];
	return function(x) {
		return x ? acc.push(x)&&f(x - 1) : acc;
	};
}();

String.prototype.ujihisa = function() {
	var x = 10;
	return this + f(x);
}

console.log("hello".ujihisa());
console.log(acc == 'dummy');
console.log({'a': 2}.a == 2);
