/**
*
*	VALIDATE: nan
*
*
*	DESCRIPTION:
*		- Validates if a value is NaN.
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
*	Copyright (c) 2014. Athan Reines.
*
*
*	AUTHOR:
*		Athan Reines. kgryte@gmail.com. 2014.
*
*/

'use strict';

/**
* FUNCTION: nan( value )
*	Validates if a value is not-a-number.
*
* @param {*} value - value to be validated
* @returns {Boolean} boolean indicating whether the value is a NaN
*/
function nan( value ) {
	return ( typeof value === 'number' || Object.prototype.toString.call( value ) === '[object Number]' ) && value.valueOf() !== value.valueOf();
} // end FUNCTION nan()


// EXPORTS //

module.exports = nan;
