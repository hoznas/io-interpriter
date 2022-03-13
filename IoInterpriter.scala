import scala.util.parsing.combinator._
/*ident wholeNumber decimalNumber stringLiteral floatingPointNumber*/

abstract class IoObject{
  def toString():String
}
trait IoCmp {
  def cmp(o:IoObject): Int 
  def lt(o:IoObject): IoObject = foo(this.cmp(o)<0)
  def le(o:IoObject): IoObject = foo(this.cmp(o)<=0)
  def gt(o:IoObject): IoObject = foo(this.cmp(o)>0)
  def ge(o:IoObject): IoObject = foo(this.cmp(o)>=0)
  def foo(b:Boolean): IoObject = if(b) IoTrue() else IoNil()
}
case class BinOp(val op:String, val lhs:IoObject, val rhs:IoObject) extends IoObject{
  override def toString():String = "[%s %s %s]".format(op,lhs,rhs)
}
case class Message(val sym:String, val args:IoObject=new IoNil()) extends IoObject{
  override def toString():String = 
    args match {
      case x:IoNil => sym
      case x:BinOp => "%s%s".format(sym, args)
      case _ => "%s[%s]".format(sym,args)
    }
}
case class Num(val v:Double) extends IoObject with IoCmp{
  override def toString():String = v.toString
  override def cmp(o:IoObject):Int = {
    o match {
      case Num(x) => this.v.compare(x)
      case _ => throw new Exception()
    }
  }
}
case class Str(val v:String) extends IoObject{
  override def toString():String = v 
}
case class IoTrue() extends IoObject{
  override def toString():String = "True"
}
case class IoNil() extends IoObject{
  override def toString():String = "Nil"
}

class Parser extends JavaTokenParsers {
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
      {  case a~(s:List[Any]) => mkBinOpTree(a,s)  }
  def binop7:Parser[IoObject] = 
    binop6~rep((";")~binop6) ^^
      {  case a~(s:List[Any]) => mkBinOpTree(a,s)  }
  def binop6:Parser[IoObject] = 
    binop5~rep((":="|"=")~binop5) ^^
      {  case a~(s:List[Any]) => mkBinOpTree(a,s)  }
  def binop5:Parser[IoObject] = 
    binop4~rep(("&&"|"||")~binop4) ^^
      {  case a~(s:List[Any]) => mkBinOpTree(a,s)  }
  def binop4:Parser[IoObject] = 
    binop3~rep(("=="|"!=")~binop3) ^^
      {  case a~(s:List[Any]) => mkBinOpTree(a,s)  }
  def binop3:Parser[IoObject] = 
    binop2~rep(("<="|">="|"<"|">")~binop2) ^^
      {  case a~(s:List[Any]) => mkBinOpTree(a,s)  }
  def binop2:Parser[IoObject] = 
    binop1~rep(("+"|"-")~binop1) ^^
      {  case a~(s:List[Any]) => mkBinOpTree(a,s)  }
  def binop1:Parser[IoObject] = 
    binop0~rep(("*"|"/"|"%")~binop0) ^^
      {  case a~(s:List[Any]) => mkBinOpTree(a,s)  }
  def binop0:Parser[IoObject] = 
    value~rep("."~value) ^^
      {  case a~(s:List[Any]) => mkBinOpTree(a,s)  }
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
    stringLiteral^^( new Str(_)) | 
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
class Evaluator {
  def eval(code:IoObject):IoObject = {
    code match {
      case Num(v) => code
      case Str(v) => code
      case BinOp(".", obj, mes:Message)
        => evalMessage(obj, mes)
      case Message(sym,args) => 
        sym match {
          case "print" =>
            println(args)
            return args
          case _ => return code
        }
      case b:BinOp => evalBinOp(b) 
      case _ => return code
    }
  }
  def evalBinOp(b: BinOp):IoObject = {
    evalBinOp1(b.op, b.lhs, b.rhs) match {
      case Some(x) => x
      case _ => {
        val elhs = eval(b.lhs)
        evalBinOp2(b.op, elhs, b.rhs) match {
          case Some(x) => x
          case _ => {
            val erhs = eval(b.rhs)
            evalBinOp3(b.op, elhs, erhs) match {
              case Some(x) => x
              case None => throw new Exception() 
            }
          }
        }
      }
    }
  }
  def evalBinOp1(op:String, lhs:IoObject, rhs:IoObject):Option[IoObject] = {
    /*"." ":="  "="  ","*/
    (op,lhs,rhs) match {
      case (",",_,_) => throw new Exception()
      case (".",_,_) => throw new Exception()
      //case (":=",_,_) => 
      //case ("=",_,_) => 
      case _ => None 
    }
    return None
  }
  def evalBinOp2(op:String, elhs:IoObject, rhs:IoObject):Option[IoObject] = {
    (op,elhs,rhs) match {
      case (":=",IoNil(),_) => Some(IoNil())
      case ("&&",IoNil(),_) => Some(IoNil())
      case ("&&",_,r) => Some(r)
      case ("||",IoNil(),r) => Some(r)
      case ("||",l,_) => Some(l)
      case _ => return None
    }
  }
  def evalBinOp3(op:String, elhs:IoObject, erhs:IoObject):Option[IoObject] = {
    (op,elhs,erhs) match {
      case ("+",Num(l),Num(r)) =>  Some(Num(l+r))
      case ("-",Num(l),Num(r)) =>  Some(Num(l-r))
      case ("*",Num(l),Num(r)) =>  Some(Num(l*r))
      case ("/",Num(l),Num(r)) =>  Some(Num(l/r))
      case ("%",Num(l),Num(r)) =>  Some(Num(l%r))
      case ("+",Str(l),r) => 
        val rr = r.toString()
        Some(Str(l.substring(0,l.length-1) + 
            rr.substring(1,rr.length)))
      case ("<=", c:IoCmp,r) => Some(c.le(r))
      case ("<",  c:IoCmp,r) => Some(c.lt(r))
      case (">=", c:IoCmp,r) => Some(c.ge(r))
      case (">",  c:IoCmp,r) => Some(c.gt(r))
      case ("==", l,r) => Some(if(l==r) IoTrue() else IoNil())
      case ("!=", l,r) => Some(if(l!=r) IoTrue() else IoNil())
      case (";",_,r) => Some(r)
      case _ => None
    }
  }
  
  def evalMessage(obj:IoObject, mes:Message): IoObject = {
    val self = eval(obj)
    val Message(sym,args) = mes  
    (self,sym) match {
      case (_,"print") => 
        println(self)
        self
      case (Num(x),"inc") => Num(x+1)
      case (Str(x),"inc") => Str(x.substring(0,x.length-1)+"1\"")
      case _ => Str(sym)
    }
  }
}

object IoInterpriter {
  def main(args:Array[String]){

    val parser = new Parser()
    val evalator = new Evaluator()
    val tests = List(
      // Num
      ("""1""","1.0"),
      ("""-123.45""","-123.45"),
      ("""1 * 2 + 3 / 4 - 5""","-2.25"),
      ("""(1 + 2) * -3 % 5""","-4.0"),
      // Str
      (""" "foo bar" """,""""foo bar""""),
      (""" "abc" + "def" """,""""abcdef""""),
      //Compare
      ("""1 < 4""","True"),
      ("""1 > 4""","Nil"),
      //Method call
      (""""abc".print""",""""abc""""),
      ("""(123).print""","123.0"),
      ("""(123<456).print""","True"),
      ("""(1234).inc.inc""","1236.0"),
      (""""abc".inc.inc""",""""abc11""""),
      // == !=
      ("""1 == 2""","Nil"),
      ("""123.45 == 123.45""","True"),
      (""" "abc" == "abc" ""","True"),
      (""" "abc" == "def" ""","Nil"),
      (""" "abc" != "abc" ""","Nil"),
      (""" "abc" != "def" ""","True"),
      // ;
      (""" 1; 2; 3; 4""","4.0"),

      // = :=
      //(""" a := 2 ""","2.0"),
      //(""" a := 2; a ""","2.0"),
      //(""" a := 2; a := 3; a ""","3.0"),

      // method chain
      // abc.def.ghi
      // abc.def().ghi()
      // abc.def(1).ghi(1)
      // abc.def(1,2,3).ghi(1,2,3)
      // if(1<3, "true", "false")
      // i:=0;while(i<10, "ok", "false")

      //tail
      ("tail","tail")
    )

    var count = 0
    for((src,mustbe) <-tests){
      println("-----------------------")
      println("[SRC]=>" + src)
      var tree = parser.parse(src)
      println("[TREE]=>"+tree)
      val result = evalator.eval(tree)
      println("[RESULT]=>"+result)
      if(result.toString()!=mustbe){
        println("[MUSTBE]=>[%s]!!!!!!!!!!!!!!".format(mustbe))
        count += 1
      }
    }
    println("-----------------------")
    if(count == 0){
      println("all tests are OK")
    }else{
      println("%d error(s) found".format(count))
    }


    
      val io_srcs = List(
        """print ( 1, 2, 3)""",
        """abc.foo( 1, 2, 3).bar(x) = ooo""",
        """if(a>5,a=10;a.print(),b=10;b.print)""",
        """ 1>2 || 3+4 """,
        """ 1<2 || 3+4 """,
        """ 1>2 && 3+4 """,
        """ 1<2 && 3+4 """,
        """1"""
    )
  }
}