-- trying another syntax approach starting with functions

func length list:(List a) -> Int -- declaration, named parameters
length []      = 0
length (x::xs) = 1 + length xs

func map f:(a->b) list:(List a) -> List b
map _ [] = []
map f (x::xs) = (f x) :: (map f xs)

length : (List a) -> Int
