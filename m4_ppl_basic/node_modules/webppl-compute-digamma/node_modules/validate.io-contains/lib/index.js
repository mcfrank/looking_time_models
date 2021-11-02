/**
*
*	VALIDATE: contains
*
*
*	DESCRIPTION:
*		- Validates if an array contains an input value.
*
*
*	NOTES:
*		[1]
*
*
*	TODO:
*		[1]
*
*
*	LICENSE:
*		MIT
*
*	Copyright (c) 2015. Athan Reines.
*
*
*	AUTHOR:
*		Athan Reines. kgryte@gmail.com. 2015.
*
*/

'use strict';

// MODULES //

var isArray = require( 'validate.io-array' ),
	isnan = require( 'validate.io-nan-primitive' );


// CONTAINS //

/**
* FUNCTION: contains( arr, value )
*	Validates if an array contains an input value.
*
* @param {Array} arr - search array
* @param {*} value - search value
* @returns {Boolean} boolean indicating if an array contains an input value
*/
function contains( arr, value ) {
	var len, i;
	if ( !isArray( arr ) ) {
		throw new TypeError( 'contains()::invalid input argument. First argument must be an array. Value: `' + arr + '`.' );
	}
	len = arr.length;
	if ( isnan( value ) ) {
		for ( i = 0; i < len; i++ ) {
			if ( isnan( arr[ i ] ) ) {
				return true;
			}
		}
		return false;
	}
	for ( i = 0; i < len; i++ ) {
		if ( arr[ i ] === value ) {
			return true;
		}
	}
	return false;
} // end FUNCTION contains()


// EXPORTS //

module.exports = contains;
