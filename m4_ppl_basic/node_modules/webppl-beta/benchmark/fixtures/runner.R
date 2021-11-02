options( digits = 16 );
library( jsonlite );

set.seed( 329 )

x = sample( seq( 0.5, 100, by=0.25 ), 1000, replace = TRUE )
y = sample( seq( 0.5, 100, by=0.25 ), 1000, replace = TRUE )
v = beta( x, y )

cat( v, sep = ",\n" )

write( toJSON( x, digits = 16, auto_unbox = TRUE ), "./test/fixtures/arg1.json" )
write( toJSON( y, digits = 16, auto_unbox = TRUE ), "./test/fixtures/arg2.json" )
write( toJSON( v, digits = 16, auto_unbox = TRUE ), "./test/fixtures/expected.json" )
