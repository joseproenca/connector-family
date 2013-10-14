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
  
  val id   = create(Interface(1),Interface(1),"id")
  val dupl = create(Interface(1),Interface(2),"dupl")
  val xr   = create(Interface(1),Interface(2),"xr")
  val xrd  = create(Interface(2),Interface(1),"xrd")
  val mrg  = create(Interface(2),Interface(1),"mrg")
  val eta  = create(Interface(0),Interface(1,-1),"eta")
  val eps  = create(Interface(-1,1),Interface(0),"eps")
  val epsd = create(Interface(1,-1),Interface(0),"epsd")
  def swap = create(Interface(2),Interface(2),"swap")
  
  def swap(i:Interface,j:Interface) = create(i++j,j++i,"swap["+i+","+j+"]")
}