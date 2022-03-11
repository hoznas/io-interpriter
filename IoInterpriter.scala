import scala.util.parsing.combinator._
/*ident wholeNumber decimalNumber stringLiteral floatingPointNumber*/

class IoObject

case class BinOp(val op:String, val car:IoObject, val cdr:IoObject) extends IoObject{
  override def toString():String = "[%s %s %s]".format(op,car,cdr)
}

case class Message(val sym:String, val args:IoObject=new IoNil()) extends IoObject{
  override def toString():String = 
    args match {
      case x:IoNil => sym
      case x:BinOp => "%s%s".format(sym, args)
      case _ => "%s[%s]".format(sym,args)
    }
}

case class Num(val v:Double) extends IoObject{
  override def toString():String = v.toString
}
case class Str(val v:String) extends IoObject{
  override def toString():String = v
}
case class IoNil() extends IoObject{
  override def toString():String = "Nil"
}



class Io extends JavaTokenParsers {
  object Op{
    def unapply(op:String):Option[String] = 
      op match {
        case "." | "+" | "-" | "*" | "/" | "%" |
             "<=" | ">=" | ">" | "<" | "==" | "!=" |
            ":=" | "&&" | "||" | "=" | ";" | "," => Some(op)
        case _ => None  
      }
  }
  def parse(src:String):IoObject = parseAll(code,src).get
  def code : Parser[IoObject] = binop7
  def binop8:Parser[IoObject] = 
    binop7~rep((",")~binop7) ^^
      {
        case a~List() => a
        case a~(s:List[Any]) => mkBinOpTree(a,s)
        case _=> throw new Exception("")
      }
  def binop7:Parser[IoObject] = 
    binop6~rep((";")~binop6) ^^
      {
        case a~List() => a
        case a~(s:List[Any]) => mkBinOpTree(a,s)
        case _=> throw new Exception("")
      }
  def binop6:Parser[IoObject] = 
    binop5~rep((":="|"=")~binop5) ^^
      {
        case a~List() => a
        case a~(s:List[Any]) => mkBinOpTree(a,s)
        case _=> throw new Exception("")
      }
  def binop5:Parser[IoObject] = 
    binop4~rep(("&&"|"||")~binop4) ^^
      {
        case a~List() => a
        case a~(s:List[Any]) => mkBinOpTree(a,s)
        case _=> throw new Exception("")
      }
  def binop4:Parser[IoObject] = 
    binop3~rep(("=="|"!=")~binop3) ^^
      {
        case a~List() => a
        case a~(s:List[Any]) => mkBinOpTree(a,s)
        case _=> throw new Exception("")
      }
  def binop3:Parser[IoObject] = 
    binop2~rep(("<="|">="|"<"|">")~binop2) ^^
      {
        case a~List() => a
        case a~(s:List[Any]) => mkBinOpTree(a,s)
        case _=> throw new Exception("")
      }
  def binop2:Parser[IoObject] = 
    binop1~rep(("+"|"-")~binop1) ^^
      {
        case a~List() => a
        case a~(s:List[Any]) => mkBinOpTree(a,s)
        case _=> throw new Exception("")
      }
  def binop1:Parser[IoObject] = 
    binop0~rep(("*"|"/"|"%")~binop0) ^^
      {
        case a~List() => a
        case a~(s:List[Any]) => mkBinOpTree(a,s)
        case _=> throw new Exception("")
      }

  def binop0:Parser[IoObject] = 
    value~rep("."~value) ^^
      {
        case a~List() => a
        case a~(s:List[Any]) => mkBinOpTree(a,s)
        case _=> throw new Exception("")
      }
  def mkBinOpTree(lhs:IoObject, lst:List[Any]):IoObject = {
    if(lst == Nil) lhs
    else{
      lst.head match {
        case (Op(op))~(rhs:IoObject) =>  
          mkBinOpTree(BinOp(op,lhs, rhs), lst.tail)
        case _ => throw new Exception("")
      }
    }
  }

  def value : Parser[IoObject] = 
    message |
    stringLiteral^^(new Str(_))| 
    floatingPointNumber^^(x => new Num(x.toDouble)) | 
    brakets 

  def message:Parser[IoObject] = 
    ident ~ opt("("~>opt(binop8)<~")")^^{
        case sym~Some(Some(x)) => new Message(sym,x)
        case sym~Some(None) => new Message(sym,new IoNil())
        case sym~None => new Message(sym,new IoNil())
        //case _ => throw new Exception("")
    }
  def brakets:Parser[IoObject] =
    "("~>binop6<~")"
}

object IoInterpriter {
  def main(args:Array[String]){
    val io_codes = List(
        """123;"he llo"; abc; 23.456""",
        """4.56; (abc)""",
        "123.45",
        "abcd",
        """ "foo bar" """,
        """abc.foo""",
        """abc.def.ghi""",
        """1 * abc.def.ghi""",
        """1 + 2""",
        """1 + 2 * 3""",
        """(1 + 2) * 3""",
        """1 + 2 > 3 * 4""",
        """1 + 2 > 3 * 4 == abc""",
        """1 + 2 > 3 * 4 && abc == xyz""",
        """a := 12 + 34""",
        """a := 1; a = "a" """,
        """print ( 1, 2, 3)""",
        """abc.foo( 1, 2, 3).bar(x) = ooo""",
        """if(a>5,a=10;a.print(),b=10;b.print)""",
        """1"""
    )
    val io = new Io()
    for(src <- io_codes)
      println(io.parse(src))


    
  }
}