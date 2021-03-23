object Main extends App {
// Zadanie 1. Zdefiniuj następujące generyczne operujące na funkcjach:
// składanie funkcji:
def compose[A, B, C](f: A => B)(g: B => C): A => C = {n:A => g(f(n))}
// iloczyn funkcji:
def prod[A, B, C, D](f: A => C, g: B => D): (A, B) => (C, D) = {(n:A,m:B) =>(f(n),g(m))}
// podniesienie operatora op: (T, T) => T
def lift[A, B, T](op: (T,T) => T)(f: A => T, g: B => T): (A,B) => T = {(n,m) => op(f(n),g(m))}


}
