-- This tests checks, whether `%memoise` functions are
-- properly memoized. If they are not, this test
-- will perform 10^20 additions and will therefore not
-- finish in reasonable time.

%memoise
n0 : Nat -> Nat
n0 = id

%memoise
n1 : Nat -> Nat
n1 i = n0 i + n0 i + n0 i + n0 i + n0 i + n0 i + n0 i + n0 i + n0 i + n0 i

%memoise
n2 : Nat -> Nat
n2 i = n1 i + n1 i + n1 i + n1 i + n1 i + n1 i + n1 i + n1 i + n1 i + n1 i

%memoise
n3 : Nat -> Nat
n3 i = n2 i + n2 i + n2 i + n2 i + n2 i + n2 i + n2 i + n2 i + n2 i + n2 i

%memoise
n4 : Nat -> Nat
n4 i = n3 i + n3 i + n3 i + n3 i + n3 i + n3 i + n3 i + n3 i + n3 i + n3 i

%memoise
n5 : Nat -> Nat
n5 i = n4 i + n4 i + n4 i + n4 i + n4 i + n4 i + n4 i + n4 i + n4 i + n4 i

%memoise
n6 : Nat -> Nat
n6 i = n5 i + n5 i + n5 i + n5 i + n5 i + n5 i + n5 i + n5 i + n5 i + n5 i

%memoise
n7 : Nat -> Nat
n7 i = n6 i + n6 i + n6 i + n6 i + n6 i + n6 i + n6 i + n6 i + n6 i + n6 i

%memoise
n8 : Nat -> Nat
n8 i = n7 i + n7 i + n7 i + n7 i + n7 i + n7 i + n7 i + n7 i + n7 i + n7 i

%memoise
n9 : Nat -> Nat
n9 i = n8 i + n8 i + n8 i + n8 i + n8 i + n8 i + n8 i + n8 i + n8 i + n8 i

%memoise
n10 : Nat -> Nat
n10 i = n9 i + n9 i + n9 i + n9 i + n9 i + n9 i + n9 i + n9 i + n9 i + n9 i

%memoise
n11 : Nat -> Nat
n11 i = n10 i + n10 i + n10 i + n10 i + n10 i + n10 i + n10 i + n10 i + n10 i + n10 i

%memoise
n12 : Nat -> Nat
n12 i = n11 i + n11 i + n11 i + n11 i + n11 i + n11 i + n11 i + n11 i + n11 i + n11 i

%memoise
n13 : Nat -> Nat
n13 i = n12 i + n12 i + n12 i + n12 i + n12 i + n12 i + n12 i + n12 i + n12 i + n12 i

%memoise
n14 : Nat -> Nat
n14 i = n13 i + n13 i + n13 i + n13 i + n13 i + n13 i + n13 i + n13 i + n13 i + n13 i

%memoise
n15 : Nat -> Nat
n15 i = n14 i + n14 i + n14 i + n14 i + n14 i + n14 i + n14 i + n14 i + n14 i + n14 i

%memoise
n16 : Nat -> Nat
n16 i = n15 i + n15 i + n15 i + n15 i + n15 i + n15 i + n15 i + n15 i + n15 i + n15 i

%memoise
n17 : Nat -> Nat
n17 i = n16 i + n16 i + n16 i + n16 i + n16 i + n16 i + n16 i + n16 i + n16 i + n16 i

%memoise
n18 : Nat -> Nat
n18 i = n17 i + n17 i + n17 i + n17 i + n17 i + n17 i + n17 i + n17 i + n17 i + n17 i

%memoise
n19 : Nat -> Nat
n19 i = n18 i + n18 i + n18 i + n18 i + n18 i + n18 i + n18 i + n18 i + n18 i + n18 i

%memoise
n20 : Nat -> Nat
n20 i = n19 i + n19 i + n19 i + n19 i + n19 i + n19 i + n19 i + n19 i + n19 i + n19 i

main : IO ()
main = do printLn $ n20 1
