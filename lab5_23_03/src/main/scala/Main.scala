object Main extends App {
// Zadanie 1.
// Zdefiniuj następujące generyczne operujące na funkcjach:
// składanie funkcji:
def compose[A, B, C](f: A => B)(g: B => C): A => C = {n:A => g(f(n))}
// iloczyn funkcji:
def prod[A, B, C, D](f: A => C, g: B => D): (A, B) => (C, D) = {(n:A,m:B) =>(f(n),g(m))}
// podniesienie operatora op: (T, T) => T
def lift[A, B, T](op: (T,T) => T)(f: A => T, g: B => T): (A,B) => T = {(n,m) => op(f(n),g(m))}
//def x[A,T](f: (A,A) => T): A => T = {}

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
def sum[A](s1: MSet[A], s2: MSet[A]): MSet[A] = {
    val abc:(Int,Int) => Int = (m,n) => m+n 
    n => lift[A,A,Int](abc)(s1,s2)(n,n)
    //n => lift[A,A,Int](_+_)(s1,s2)(n,n) // alternatywny zapis
    }
def diff[A](s1: MSet[A], s2: MSet[A]): MSet[A] = {
    val r: (Int, Int)=> Int = (n,m) => n-m max 0
    n => lift(r)(s1,s2)(n,n)
    }
def mult[A](s1: MSet[A], s2: MSet[A]): MSet[A] = {
    val m: (Int, Int) => Int = (n,m) => n min m
    n => lift[A,A,Int](m)(s1,s2)(n,n)
    }











// Zadanie 3. Zdefiniuj generyczną funkcję rekurencyjną:
// def insertInto[A](l: List[A], el: A)(leq: (A, A) => Boolean): List[A] = /* ... */
// która wstawi element a do sekwencji l zgodnie z porządkiem określonym przez leq.
// Przykład:
// Dla: l = List(1, 2, 4, 6), el = 3 i leq = (_ < _), funkcja powinna zwrócić: List(1, 2, 3, 4, 6).
def insertInto[A](l: List[A], el: A)(leq: (A, A) => Boolean): List[A] = {
    def poboczna[A](l: List[A], el: A, l2: List[A])(leq: (A, A) => Boolean): List[A] = {
        def p[A](l: List[A], l2: List[A]): List[A] = l match{
            case List() => l2
            case a :: b => p(b,a::l2)
        }
        l match{
            case List() => l2
            case a :: b if leq(a,el) => p(b,el::a::l2)
        }
        
    }
    l match{
        case List() => el::l
        case _ => poboczna(l,el,List[A]())(leq)
    }
}

// Zadanie 4. Zdefiniuj generyczną funkcję rekurencyjną:
// def compress[A](l: List[A]): List[(A, Int)] = /* ... */
// która zastąpi każdy element listy l parą (element, długość_podciągu). Zdefiniuj funkcję z użyciem rekurencji ogonowej.
// Nie używaj metod head i tail.
// Przykład:
// Dla: l = List('a', 'a', 'b', 'c', 'c', 'c', 'a', 'a', 'b', 'd'), funkcja powinna zwrócić: List(('a', 2), ('b', 1), ('c', 3), ('a', 2), ('b', 1), ('d', 1)).
def compress[A](l: List[A]): List[(A, Int)] ={
    def poboczna[A](l: List[A], l2:List[(A,Int)] ): List[(A, Int)] = l match{
        case List() => l2.reverse
        case a::b => l2 match {
            case List() => poboczna(b, (a, 1)::l2)
            case x::y if x._1 == a => poboczna(b,(x._1,x._2+1)::y)
            case _ => poboczna(b, (a,1)::l2)
        } 
    }
    poboczna(l, List())
}
val l = List('a', 'a', 'b', 'c', 'c', 'c', 'a', 'a', 'b', 'd')
println(compress(l))
// Zadanie 5. Zdefiniuj generyczną funkcję rekurencyjną:
// def compute[A, B](l: List[A], init: B)(op: (A, B) => B): B = /* ... */
// która korzystając z wartości początkowej oraz funkcji op oblicz "wartość" ciągu l.
// Zdefiniuj funkcję z użyciem rekurencji ogonowej.
// Przykłady:
// Dla: l = List(1, 2, 3, 4), init = 0, op = (_ + _), funkcja powinna zwrócić: 10.
// Dla: l = List(1, 2, 3, 4), init = 1, op = (_ * _), funkcja powinna zwrócić: 24.
// Dla: l = List("kota"," ","ma"," ","ala"), init = "", op = (_ + _), funkcja powinna zwrócić: "ala ma kota".
def compute[A, B](l: List[A], init: B)(op: (A, B) => B): B = {
    def poboczna[A, B](l: List[A], buf: B)(op: (A, B) => B): B = l match{
        case List() => buf
        case a::b => poboczna(b,op(a, buf))(op)
    }
    l match {
        case List() => init
        case a::b => poboczna(b,op(a, init))(op)
    }
}
println(compute(List(1, 2, 3, 4),0)(_ + _))
println(compute(List(1, 2, 3, 4),1)(_ * _))
println(compute(List("kota"," ","ma"," ","ala"),"")(_ + _))
}
