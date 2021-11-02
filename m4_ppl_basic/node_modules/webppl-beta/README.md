beta
===
[![NPM version][npm-image]][npm-url] [![Build Status][build-image]][build-url] [![Coverage Status][coverage-image]][coverage-url] [![Dependencies][dependencies-image]][dependencies-url]

> [Beta function][beta-function].


The [Beta function][beta-function], also called the Euler integral, is defined as

<div class="equation" align="center" data-raw-text="
	\operatorname{Beta}(x,y) = \int_0^1t^{x-1}(1-t)^{y-1}\,\mathrm{d}t" data-equation="eq:beta_function">
	<img src="https://cdn.rawgit.com/math-io/beta/9d5602c468c0b1b4317db9ade03642ce8edbce47/docs/img/eqn1.svg" alt="Equation for the beta function.">
	<br>
</div>

It is related to the [Gamma function][gamma-function] via the following equation

<div class="equation" align="center" data-raw-text="
\operatorname{Beta}(x,y)=\dfrac{\Gamma(x)\,\Gamma(y)}{\Gamma(x+y)} \!
" data-equation="eq:beta_function2">
	<img src="https://cdn.rawgit.com/math-io/beta/9d5602c468c0b1b4317db9ade03642ce8edbce47/docs/img/eqn2.svg" alt="Beta function expressed in terms of the Gamma function.">
	<br>
</div>

## Installation

``` bash
$ npm install math-beta
```


## Usage

``` javascript
var beta = require( 'math-beta' );
```


#### beta( x, y )

Evaluates the the [Beta function][beta-function].

``` javascript
var val = beta( 0, 0 );
// returns +Infinity

val = beta( 1, 1 );
// returns 1

val = beta( -1, 2 );
// return NaN

val = beta( 5, 0.2 );
// returns ~3.382

val = beta( 4, 1 );
// returns 0.25
```


## Examples

``` javascript
var beta = require( 'math-beta' );

for ( var x = 0; x < 10; x++ ) {
	for ( var y = 10; y > 0; y-- ) {
		console.log( 'x: %d, \t y: %d, \t f(x,y): %d', x, y, beta( x, y ) );
	}
}
```

To run the example code from the top-level application directory,

``` bash
$ node ./examples/index.js
```


---
## Tests

### Unit

This repository uses [tape][tape] for unit tests. To run the tests, execute the following command in the top-level application directory:

``` bash
$ make test
```

All new feature development should have corresponding unit tests to validate correct functionality.


### Test Coverage

This repository uses [Istanbul][istanbul] as its code coverage tool. To generate a test coverage report, execute the following command in the top-level application directory:

``` bash
$ make test-cov
```

Istanbul creates a `./reports/coverage` directory. To access an HTML version of the report,

``` bash
$ make view-cov
```


### Browser Support

This repository uses [Testling][testling] for browser testing. To run the tests in a (headless) local web browser, execute the following command in the top-level application directory:

``` bash
$ make test-browsers
```

To view the tests in a local web browser,

``` bash
$ make view-browser-tests
```

<!-- [![browser support][browsers-image]][browsers-url] -->


---
## License

[MIT license](http://opensource.org/licenses/MIT).


## Copyright

Copyright &copy; 2016. The [Compute.io][compute-io] Authors.


[npm-image]: http://img.shields.io/npm/v/math-beta.svg
[npm-url]: https://npmjs.org/package/math-beta

[build-image]: http://img.shields.io/travis/math-io/beta/master.svg
[build-url]: https://travis-ci.org/math-io/beta

[coverage-image]: https://img.shields.io/codecov/c/github/math-io/beta/master.svg
[coverage-url]: https://codecov.io/github/math-io/beta?branch=master

[dependencies-image]: http://img.shields.io/david/math-io/beta.svg
[dependencies-url]: https://david-dm.org/math-io/beta

[dev-dependencies-image]: http://img.shields.io/david/dev/math-io/beta.svg
[dev-dependencies-url]: https://david-dm.org/dev/math-io/beta

[github-issues-image]: http://img.shields.io/github/issues/math-io/beta.svg
[github-issues-url]: https://github.com/math-io/beta/issues

[tape]: https://github.com/substack/tape
[istanbul]: https://github.com/gotwarlost/istanbul
[testling]: https://ci.testling.com

[beta-function]: http://en.wikipedia.org/wiki/Beta_function
[compute-io]: https://github.com/compute-io/
[gamma-function]: https://en.wikipedia.org/wiki/Gamma_function
