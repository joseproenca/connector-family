package connectorFamily.connector

case class SimpleRep(val name:String) extends Rep[SimpleRep] {
  def *(other:SimpleRep): SimpleRep = SimpleRep(name ++ "*" ++ other.name) 
  def &(other:SimpleRep): SimpleRep = SimpleRep(name ++ " ; " ++ other.name) 
  def +(other:SimpleRep): SimpleRep = SimpleRep(name ++ "+" ++ other.name) 
  def inv: SimpleRep = SimpleRep(name ++ "'") 
  override def toString = name 
}

object Examples {
	private def create(in:Interface,out:Interface,str:String) =
		new Connector(in,out,SimpleRep(str))
	
  val sync   = create(Interface(1),Interface(1),"sync")
  val lossy  = create(Interface(1),Interface(1),"lossy")
  val fifo   = create(Interface(1),Interface(1),"fifo")
  val fifof  = create(Interface(1),Interface(1),"fifof")
  val sdrain = create(Interface(2),Interface(0),"sdrain")

  val id0 = create(Interface(0),Interface(0),"id0")
  val id   = create(Interface(1),Interface(1),"id")
  val dupl = create(Interface(1),Interface(2),"dupl")
  val xr   = create(Interface(1),Interface(2),"xr")
  val mrg  = create(Interface(2),Interface(1),"mrg")
  val flow = create(Interface(1),Interface(0),"flow")
  val noflow  = create(Interface(1),Interface(0),"noflow")
  val xrd  = create(Interface(2),Interface(1),"xrd")
  val flowd= create(Interface(1),Interface(0),"flowd")
  val noflowd = create(Interface(1),Interface(0),"noflowd")
  val eta  = create(Interface(0),Interface(1,-1),"eta")
  val eps  = create(Interface(-1,1),Interface(0),"eps")
  val etar = create(Interface(0),Interface(-1,1),"etar")
  val epsr = create(Interface(1,-1),Interface(0),"epsr")
  def swap = create(Interface(2),Interface(2),"swap")
  
  def swap(i:Interface,j:Interface) = create(i++j,j++i,"swap["+i+","+j+"]")
  def xr(i:Int) = create(Interface(1),Interface(i),"xr["+i+"]")
  def dupl(i:Int) = create(Interface(1),Interface(i),"dupl["+i+"]")
  def mrg(i:Int) = create(Interface(i),Interface(1),"mrg["+i+"]")
}