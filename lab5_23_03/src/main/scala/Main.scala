object Main extends App {
// Zadanie 1.
// Zdefiniuj następujące generyczne operujące na funkcjach:
// składanie funkcji:
def compose[A, B, C](f: A => B)(g: B => C): A => C = {n:A => g(f(n))}
// iloczyn funkcji:
def prod[A, B, C, D](f: A => C, g: B => D): (A, B) => (C, D) = {(n:A,m:B) =>(f(n),g(m))}
// podniesienie operatora op: (T, T) => T
def lift[A, B, T](op: (T,T) => T)(f: A => T, g: B => T): (A,B) => T = {(n,m) => op(f(n),g(m))}


// Zadanie 2.
// Zdefiniuj następujące generyczne operujące na funkcjach:
// Niech MSet[A] oznacza multi-zbiór (zbiór w którym elementy mogą się powtarzać) typu A.
type MSet[A] = A => Int
// Czyli jest to funkcja zwracająca liczbę wystąpienia elementu typu A w danym multizbiorze, np.
val a:MSet[Int] = (n: Int) => n match {
 case 1 => 2
 case 3 => 1
 case _ => 0
}
// Korzystając z funkcji w podpunkcie a zdefiniuj funkcję wykonujące operację:
// sumy, różnicy oraz części wspólnej dla wielozbiorów:
def sum[A](s1: MSet[A], s2: MSet[A]): MSet[A] = {lift[A,Int](_+_)(s1,s2)}
// def diff[A](s1: MSet[A], s2: MSet[A]): MSet[A] = {
//   //val r: (Int, Int)=> Int = (n,m) => n-m max 0
//   //lift(r)(s1,s2)
//   }
// def mult[A](s1: MSet[A], s2: MSet[A]): MSet[A] = {lift[A,Int]((n,m) => n min m)(s1,s2) }











// Zadanie 3. Zdefiniuj generyczną funkcję rekurencyjną:
// def insertInto[A](l: List[A], el: A)(leq: (A, A) => Boolean): List[A] = /* ... */
// która wstawi element a do sekwencji l zgodnie z porządkiem określonym przez leq.
// Przykład:
// Dla: l = List(1, 2, 4, 6), el = 3 i leq = (_ < _), funkcja powinna zwrócić: List(1, 2, 3, 4, 6).

// Zadanie 4. Zdefiniuj generyczną funkcję rekurencyjną:
// def compress[A](l: List[A]): List[(A, Int)] = /* ... */
// która zastąpi każdy element listy l parą (element, długość_podciągu). Zdefiniuj funkcję z użyciem rekurencji ogonowej. Nie używaj metod head i tail.
// Przykład:
// Dla: l = List('a', 'a', 'b', 'c', 'c', 'c', 'a', 'a', 'b', 'd'), funkcja powinna zwrócić: List(('a', 2), ('b', 1), ('c', 3), ('a', 2), ('b', 1), ('d', 1)).

// Zadanie 5. Zdefiniuj generyczną funkcję rekurencyjną:
// def compute[A, B](l: List[A], init: B)(op: (A, B) => B): B = /* ... */
// która korzystając z wartości początkowej oraz funkcji op oblicz "wartość" ciągu l. Zdefiniuj funkcję z użyciem rekurencji ogonowej.
// Przykłady:
// Dla: l = List(1, 2, 3, 4), init = 0, op = (_ + _), funkcja powinna zwrócić: 10.
// Dla: l = List(1, 2, 3, 4), init = 1, op = (_ * _), funkcja powinna zwrócić: 24.
// Dla: l = List("kota"," ","ma"," ","ala"), init = "", op = (_ + _), funkcja powinna zwrócić: "ala ma kota".

}
