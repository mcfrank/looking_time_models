'use strict';

// MODULES //

var DIGAMMA = require( './number.js' );


// DIGAMMA FUNCTION //

/**
* FUNCTION: digamma( out, matrix )
*	Evaluates the digamma function for each matrix element.
*
* @param {Matrix} out - output matrix
* @param {Matrix} matrix - input matrix
* @returns {Matrix} output matrix
*/
function digamma( y, x ) {
	var len = x.length,
		i;
	if ( y.length !== len ) {
		throw new Error( 'digamma()::invalid input arguments. Input and output matrices must be the same length.' );
	}
	for ( i = 0; i < len; i++ ) {
		y.data[ i ] = DIGAMMA( x.data[ i ] );
	}
	return y;
} // end FUNCTION digamma()


// EXPORTS //

module.exports = digamma;
