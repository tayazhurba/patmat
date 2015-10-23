package patmat

import common._

import scala.collection.immutable.Nil

object Huffman {

  /**
   * Код Хафмана представлен бинарным деревом.
   *
   * Каждый `Leaf` узел дерева представляет символ алфавита, которое дерево может закодировать.
   * Вес `Leaf` -частотность символа в тексте.
   *
   * Ветви дерева хафмана, `Fork` узлы, представляют множество, содержащее все символы , присутствующие в дереве ниже него.
   * Вес узла `Fork` равен сумме весом его листьев
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

    def weight(tree: CodeTree): Int = tree match{
      case Fork(_, _, _, weight) => weight
      case Leaf(_, weight) => weight
    };
  
    def chars(tree: CodeTree): List[Char] = tree match{
      case Fork(_, _, x, _) => x
      case Leaf(x, _) => List(x)
    };
  
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Часть 2: Генерация дерева Хафмана

  def string2Chars(str: String): List[Char] = str.toList

  /**
   * Эта функция вычисялет для каждого уникального символа в списке `chars` число
   * раз, которое он встречается. Например, вызов
   *
   *   times(List('a', 'b', 'a'))
   *
   * может вернуть следующее (порядок результирующего списка не важен):
   *
   *   List(('a', 2), ('b', 1))
   *
   * Тип `List[(Char, Int)]` обозначает список пар (кортежей), где каждая пар состоит из символа и числа.
   * Пары могут быть сконструированы с помощью скобок:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * Для того чтобы получить доступ к двум элементам пары, вам необходимо использовать аксессоры `_1` и `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Другой способ деконструировать пару - используя pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    def timeOfChar(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {
      if (chars.isEmpty) acc
      else timeOfChar(chars.tail.filter(_ != chars.head), acc :+ (chars.head, 1 + chars.tail.count(_ == chars.head)))
    }
    timeOfChar(chars, List())
  }

  /**
   * Возвращает список узлов `Leaf` по заданной таблице частотности `freqs`.
   *
   * Возвращает список, который должен быть упорядочен по возрастанию весов (т.е.
   * голова списка должна иметь наименьший вес), где вес узла - есть частота символа.
   */
    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
      freqs.sortWith((elem1,elem2) => elem1._2 < elem2._2).map((elem) => Leaf(elem._1,elem._2))
    }
  /**
   * Проверяет, содержит ли список `trees` только одно кодовое дерево.
   */
    def singleton(trees: List[CodeTree]): Boolean = {
    trees.size == 1
  }

  /**
   * Параметр `trees` этой функции - список деревьев кода, упорядоченных по возрастанию весов.
   *
   * Эта функция берет два первых элемента списка `trees` и комбинирует их в отдельный узел
   * `Fork`. Этот узел затем добавляется обратно к оставшимся элементам
   * `trees` в позицию, так чтобы сохранить порядок возрастания весов.
   *
   * Если `trees` является списком, меньше чем из двух элементов, список дожен вернуться неизменным
   */
    def combine(trees: List[CodeTree]): List[CodeTree] = trees match{
    case left::right::chars => (makeCodeTree(left, right)::chars).sortWith((elem1,elem2) => weight(elem1) < weight(elem2))
    case _ => trees
  }

  /**
   * Эта функция будет вызвана следующим образом: until(singleton, combine)(trees)
   *
   * где `trees` имеет тип `List[CodeTree]`, `singleton` и `combine` - это две функции определенные выше.
   *
   * При таком вызове `until` должен вызывать две функции до тех пор пока список
   * кодовых деревьев не будет содержать только одно дерево, и затем вернет этот список.
   *
   * Подсказка: перед написанием реализаци,
   *  - начните с определения типов параметров, таким образом, чтобы удовлетворять сигнатуре вызова сверху.
   *    Типы параметров ф-ции `until` должны совпападать с типами аргументов вызова-примера.
   *    Еще определите тип возвращаемого значения функции `until`.
   *  - постарайтесь подобрать значимые имена параметров для `xxx`, `yyy` и `zzz`.
   */
    def until(single: List[CodeTree] => Boolean, listTrees: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
      if (single(trees)) trees
      else until(single,listTrees)(combine(trees))
  }

    def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  type Bit = Int

    def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
      def decode1(chars: List[Char], curTree: CodeTree, bits: List[Bit]): List[Char] = curTree match{
        case Leaf(c,_) if (bits.isEmpty) => (c::chars).reverse
        case Leaf(c,_) => decode1(c::chars, tree, bits)
        case Fork(l,_,_,_) if (bits.head == 0) => decode1(chars, l, bits.tail)
        case Fork(_,r,_,_) if (bits.head == 1) => decode1(chars, r, bits.tail)
      }
      decode1(Nil, tree, bits);
    }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Напишите функцию, которая возвращает декодированный секрет
   */
    def decodedSecret: List[Char] = decode(frenchCode, secret)
  

  // Часть 4a: Кодирование с использованием дерева Хафмана

  /**
   * ЭТа функция кодирует `text` с помощью кодового дерева `tree` в
   * последовательность битов.
   */
    def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
      def encode1(curTree: CodeTree, bits: List[Bit])(text: List[Char]): List[Bit] = curTree match{
        case Leaf(c,_) if (text.tail == Nil) => bits.reverse
        case Leaf(c,_) => encode1(tree, bits)(text.tail)
        case Fork(l,r,_,_) if (chars(l).contains(text.head)) => encode1(l,0::bits)(text)
        case Fork(l,r,_,_) => encode1(r,1::bits)(text)
      }
      encode1(tree,Nil)(text)
    }

  val hc = Huffman.decodedSecret

  def encodeSecret: List[Bit] = encode(frenchCode)(hc)

  // Часть 4b: Кодирование, использующее таблицу

  type CodeTable = List[(Char, List[Bit])]

  /**
   * ЭТа функция возвращает последовательность битов, представляющая символ `char` в
   * кодовую таблицу `table`.
   */
    def codeBits(table: CodeTable)(char: Char): List[Bit] = ???
  
  /**
   * Имя кодовое дерево, создайте кодовую таблицу, которая содержит, для каждого символа в
   * кодовом дереве, последовательность бит, представляющее этот символ.
   *
   * Подсказка: подумайте о рекурсивном решении: каждое поддерево кодового дерева `tree` - само по себе 
   * валидное кодовое дерево, которое может быть представлено в качестве кодовой таблицы. Используя кодовые таблицы
   * поддеревьев, подумайте о том, как построить кодовую таблицу для всего дерева.
   */
    def convert(tree: CodeTree): CodeTable = ???
  
  /**
   * Эта функция принимает две кодовой таблицы и делает слияние их в одну. В зависимости от того, как вы 
   * используете ее в методе `convert`, этот метод merge может также производить некоторые трансформации 
   * над двумя параметрами кодовых таблиц.
   */
    def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = ???
  
  /**
   * Эта функция кодирует `text` согласно кодовому дереву `tree`.
   *
   * Для ускорения процесса кодирования, сначала она конвертирует кодовое дерево в кодовую таблицу
   * и затем использует ее для произведения непосредственно кодирования.
   */
    def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = ???
  }

object Main extends App{
  println("hello =)")

  // println(Huffman.makeOrderedLeafList(Huffman.times("hello".toList)))

  val hc = Huffman.decodedSecret
  println(hc)

  val he = Huffman.encodeSecret
  println(he)
//
//  val code = Huffman.encode(Huffman.frenchCode)(hc)
//  println(code)
//
//  val table = Huffman.convert(Huffman.frenchCode)
//  println(table)
//
//  val ch = Huffman.codeBits(table)('d')
//  println(ch)
//
//  val q = Huffman.quickEncode(Huffman.frenchCode)("huffmanestcool".toList)
//  println(q)

}