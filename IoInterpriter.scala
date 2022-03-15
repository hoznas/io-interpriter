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
case class Message(val sym:String, val args:Option[IoObject]) extends IoObject{
  override def toString():String = 
    args match {
      case None => sym
      case Some(x:BinOp) => "%s%s".format(sym, x)
      case Some(x) => "%s[%s]".format(sym,x)
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
trait Callable{
  val args:List[String]
  val body:IoObject
}
/*
case class Function(argTree:Option[IoObject]) extends IoObject with Callable{
  val (args, body) = argsAndBody(argTree.get)
  def argsAndBody(argTree:IoObject, args:List[String]=Nil):(List[String],IoObject) = {
    argTree match {
      case BinOp(",",Message(sym, None),tail) => argsAndBody(tail,sym::args)
      case last => (args.reverse, last)
    }
  }
  override def toString():String ={
    "fun(%s){%s}".format(args.mkString("",",",""),body)
  }
}*/

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
        case sym~Some(Some(x)) => new Message(sym,Some(x))
        case sym~Some(None) => new Message(sym,None)
        case sym~None => new Message(sym,None)
        //case _ => throw new Exception("")
    }
  def brakets:Parser[IoObject] =
    "("~>binop6<~")"
}
class Scope(val sup: Option[Scope] = None) {
  val map:scala.collection.mutable.Map[String,IoObject] =
    scala.collection.mutable.Map()

  def get(sym: String): Option[IoObject] = {
    map.get(sym) match {
      case obj:Some[IoObject] => obj
      case None if sup!=None  => sup.get.get(sym)
      case None               => None
    }
  }
  def define(sym:String, obj:IoObject):Unit = {
    if(map.get(sym) == None) map(sym) = obj
    else throw new Exception()
  }
  def update(sym:String, obj:IoObject):Unit = {
    map.get(sym) match {
      case Some(_)            => map(sym) = obj
      case None if(sup!=None) => sup.get.update(sym,obj)
      case _                  => throw new Exception()
    }
  }
  def subScope():Scope = {
    new Scope(Some(this))
  }
}

object Evaluator{
  def eval(code:IoObject):IoObject = {
    
    val e = new Evaluator()
    val s = new Scope
    e.eval(s,code)
  }
}

class Evaluator {
  def eval(s:Scope,code:IoObject):IoObject = {
    code match {
      case Num(v) => code
      case Str(v) => code
      case BinOp(".", obj, mes:Message)
        // obj.method(arg)
        => evalMessage(s,obj, mes)
      case Message(sym,None) => 
        // variable
        s.get(sym) match {
          case Some(x) => x
          case None => IoNil() 
        } 
      case Message(sym,args) => 
        // sym(arg)
        sym match {
          case "to_s" => Str(args.toString()) // test
          //case "fun"  => Function(args)
          case _ => return code
        }
      
      case b:BinOp => evalBinOp(s,b) 
      case _ => return code
    }
  }
  def evalBinOp(s:Scope,b: BinOp):IoObject = {
    evalBinOp1(s,b.op, b.lhs, b.rhs) match {
      case Some(x) => x
      case _ => {
        val elhs = eval(s,b.lhs)
        evalBinOp2(s,b.op, elhs, b.rhs) match {
          case Some(x) => x
          case _ => {
            val erhs = eval(s,b.rhs)
            evalBinOp3(s,b.op, elhs, erhs) match {
              case Some(x) => x
              case None => throw new Exception() 
            }
          }
        }
      }
    }
  }
  def evalBinOp1(s:Scope,op:String, lhs:IoObject, rhs:IoObject):Option[IoObject] = {
    // . := = ,
    (op,lhs,rhs) match {
      case (",",_,_) => throw new Exception() 
      case (".",_,_) => throw new Exception()
      case (":=",Message(sym,x),obj) =>
        val erhs = eval(s,obj)
        s.define(sym,erhs)
        Some(erhs)
      case ("=",Message(sym,x),obj) =>
        val erhs = eval(s,obj)
        s.update(sym,erhs)
        Some(erhs)
      case _ => None 
    }
  }
  def evalBinOp2(s:Scope,op:String, elhs:IoObject, rhs:IoObject):Option[IoObject] = {
    (op,elhs,rhs) match {
      case (":=",IoNil(),_) => Some(IoNil())
      case ("&&",IoNil(),_) => Some(IoNil())
      case ("&&",_,r) => Some(eval(s,r))
      case ("||",IoNil(),r) => Some(eval(s,r))
      case ("||",l,_) => Some(l)
      case _ => return None
    }
  }
  def evalBinOp3(s:Scope,op:String, elhs:IoObject, erhs:IoObject):Option[IoObject] = {
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
  
  def evalMessage(s:Scope,obj:IoObject, mes:Message): IoObject = {
    val self = eval(s,obj)
    val Message(sym,args) = mes  
    (self,sym) match { 
      case (_,"print") => // test
        println(self)
        self
      case (Num(x),"inc") => Num(x+1)// test
      case (Str(x),"inc") => Str(x.substring(0,x.length-1)+"1\"")// test
      case _ => Str(sym)
    }
  }
}

object IoInterpriter {
  def main(args:Array[String]){

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

      //call
      //("""to_s(123.45+0.006)""", """ "123.456" """ ),

      // = :=
      (""" a := 1 + 2 ""","3.0"),
      (""" a := 1 + 2; a ""","3.0"),
      (""" a := 1; a = 2+3 ""","5.0"),
      (""" a := 1; a = 2+3; a ""","5.0"),

      //(""" fun(a,b,a+b) ""","?"),

      // method chain
      // abc.def.ghi
      // abc.def().ghi()
      // abc.def(1).ghi(1)
      // abc.def(1,2,3).ghi(1,2,3)

      // syntax
      // if(1<3, "true", "false")
      // i:=0;while(i<10, "ok", "false")

      //tail
      (""""tail"""",""""tail"""")
    )

    var count = 0
    val parser = new Parser()
    for((src,mustbe) <-tests){
      println("-----------------------")
      println("[SRC]=>" + src)
      val tree = parser.parse(src)
      println("[TREE]=>"+tree)
      val result = Evaluator.eval(tree)
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

/*
    val root = new Scope()
    root.define("one",Num(1))
    root.define("two",Num(2))
    val sub = root.subScope()
    sub.define("three",Num(3))
    println(root.get("one"),root.get("two"),root.get("three"))
    println(sub.get("one"),sub.get("two"),sub.get("three"))
    root.update("one", Str("ICHI"))
    sub.update("two",Str("Ni"))
    println(sub.get("one"),sub.get("two"),sub.get("three"))
    //root.update("three",Num(33))
    */

    //"""print ( 1, 2, 3)""",
    //"""abc.foo( 1, 2, 3).bar(x) = ooo""",
    //"""if(a>5,a=10;a.print(),b=10;b.print)""",
  }
}