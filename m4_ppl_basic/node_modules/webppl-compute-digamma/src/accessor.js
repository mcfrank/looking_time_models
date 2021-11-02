'use strict';

// FUNCTIONS

var DIGAMMA = require( './number.js' );


// DIGAMMA FUNCTION //

/**
* FUNCTION: digamma( out, arr, accessor )
*	Computes the digamma function for each array element using an accessor function.
*
* @param {Array|Int8Array|Uint8Array|Uint8ClampedArray|Int16Array|Uint16Array|Int32Array|Uint32Array|Float32Array|Float64Array} out - output array
* @param {Array} arr - input array
* @param {Function} accessor - accessor function for accessing array values
* @returns {Number[]|Int8Array|Uint8Array|Uint8ClampedArray|Int16Array|Uint16Array|Int32Array|Uint32Array|Float32Array|Float64Array} output array
*/
function digamma( y, x, clbk ) {
	var len = x.length,
		v, i;
	for ( i = 0; i < len; i++ ) {
		v = clbk( x[ i ], i );
		if ( typeof v === 'number' ) {
			y[ i ] = DIGAMMA( v );
		} else {
			y[ i ] = NaN;
		}
	}
	return y;
} // end FUNCTION digamma()


// EXPORTS //

module.exports = digamma;
