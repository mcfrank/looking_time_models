Contains
===
[![NPM version][npm-image]][npm-url] [![Build Status][travis-image]][travis-url] [![Coverage Status][coveralls-image]][coveralls-url] [![Dependencies][dependencies-image]][dependencies-url]

> Validates if an array contains an input value.


## Installation

``` bash
$ npm install validate.io-contains
```

For use in the browser, use [browserify](https://github.com/substack/node-browserify).


## Usage

``` javascript
var contains = require( 'validate.io-contains' );
```

#### contains( arr, value )

Validates if an `array` contains an input `value`.

``` javascript
var arr = [ 1, '2', 3 ];

var bool = contains( arr, '2' );
// returns true

bool = contains( arr, 2 );
// returns false
```

__Note__: if not provided an `array`, the function will throw an `Error`.


## Examples

``` javascript
var contains = require( 'validate.io-contains' );

var arr = [ 1, '2', NaN, null, {} ];

console.log( contains( arr, '2' ) );
// returns true

console.log( contains( arr, NaN ) );
// returns true

console.log( contains( arr, 2 ) );
// returns false
```

To run the example code from the top-level application directory,

``` bash
$ node ./examples/index.js
```


## Tests

### Unit

Unit tests use the [Mocha](http://mochajs.org) test framework with [Chai](http://chaijs.com) assertions. To run the tests, execute the following command in the top-level application directory:

``` bash
$ make test
```

All new feature development should have corresponding unit tests to validate correct functionality.


### Test Coverage

This repository uses [Istanbul](https://github.com/gotwarlost/istanbul) as its code coverage tool. To generate a test coverage report, execute the following command in the top-level application directory:

``` bash
$ make test-cov
```

Istanbul creates a `./reports/coverage` directory. To access an HTML version of the report,

``` bash
$ make view-cov
```


---
## License

[MIT license](http://opensource.org/licenses/MIT). 


## Copyright

Copyright &copy; 2015. Athan Reines.


[npm-image]: http://img.shields.io/npm/v/validate.io-contains.svg
[npm-url]: https://npmjs.org/package/validate.io-contains

[travis-image]: http://img.shields.io/travis/validate-io/contains/master.svg
[travis-url]: https://travis-ci.org/validate-io/contains

[coveralls-image]: https://img.shields.io/coveralls/validate-io/contains/master.svg
[coveralls-url]: https://coveralls.io/r/validate-io/contains?branch=master

[dependencies-image]: http://img.shields.io/david/validate-io/contains.svg
[dependencies-url]: https://david-dm.org/validate-io/contains

[dev-dependencies-image]: http://img.shields.io/david/dev/validate-io/contains.svg
[dev-dependencies-url]: https://david-dm.org/dev/validate-io/contains

[github-issues-image]: http://img.shields.io/github/issues/validate-io/contains.svg
[github-issues-url]: https://github.com/validate-io/contains/issues
