module Construct where
clSy name symbol = "func "++name++"(x){
    y := all
    bound y "++symbol++" x
    return y
}"
clSys = [clSy "bGt" ">",clSy "bLt" "<",clSy "bGte" ">=",clSy "bLte" "<=","func bBetween(x,y){
    z := all
    bound z > x && z < y
    return z
}"] >>= id

