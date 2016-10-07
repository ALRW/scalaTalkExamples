
// verbose conditional
def isCondition: Boolean = {
  if(condition){
    return true
  } else {
    return false
  }
}


// basic Case Class

case class AppointmentRequest (SlotId: String,
                              FirstName: String,
                              LastName: String,
                              GWF: String,
                              Passport: String,
                              NationalityCode: String,
                              DateOfBirth: String,
                              VisaSubTypeId: String)


// exmaple extractors

class Block(val name: String)

object Block {
  def unapply(block: Block): Option[String] = Some(block.name)
}

class BigBlock(val name: String, val id: Int, val attributes: Map[String, String])

object BigBlock {
  def unapply(bigBlock: BigBlock): Option[(String, Int, Map[String, String])] =
    Some((bigBlock.name, bigBlock.id, bigBlock.attributes))
}

class BooleanBlock(val name: String, val matchingProbability: Double)

object BooleanBlock {
  def unapply(booleanBlock: BooleanBlock): Boolean =
    booleanBlock.matchingProbability > 0.5
}

// Useful example of an extractor

object EMail {

  // The injection method (optional but good practice)
  def apply(user: String, domain: String) = user +"@"+ domain

  // The extraction method (mandatory)
  def unapply(str: String): Option[(String, String)] = {
    val parts = str split "@"
    if (parts.length == 2) Some(parts(0), parts(1)) else None
  }
}

// example sequence extractor

object ::> {def unapplySeq[T](list: Seq[T]): Option[(T, Seq[T])] =
  Some((list.last, list.init))}


// example pattern matching ***everything***

val Pattern = "(pat)(tern)".r
def matcher(something: Any) = something match {
  case 1 | 2 | 3 => "We check for multiple individual items (can't use extractors for this)"
  case "String" | "otherString" => "Strings!"
  case x: Int if x > 3 => "catching any other Int with a guard clause"
  case Pattern(first, second) => s"matching against a Regex $first$second"
  case Block(name) => s"matching against our Block example ($name)"
  case BigBlock(name,id,_) => s"matching against our BigBlock example ($name, $id)"
  case block @ BooleanBlock() => s"using a boolean matcher and assigning the result to a var $block"
  case x ::> _ => s"Using our custom extractor to return the last element of a Seq ($x)"
  case x :: Nil => s"Matches a normal Seq of one element $x"
  case Stream(x, _*) => s"collections can be deconstructed too"
  case ar @ AppointmentRequest(_,_,_,_,_,_,_,_) => s"lastly our case class: $ar"
}



// comparison using pattern matching anonymous funtion

val popularProducts = ("SetM", 4) :: ("FLRP", 1) :: ("EEAQP", 0) :: Nil

// example without pattern matching anonymous funstion

def popular(products: List[(String, Int)]): List[String] =
  products.filter(_._2 > 2).map(_._1)

// example of pattern matching anonymous function

def popular(products: List[(String, Int)]): List[String] =
  products filter { case (_, popularity) => popularity > 2} map {case (name, _) => name}



