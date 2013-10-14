package connectorFamily.featureModel

/**
 * Selection of features.
 * It can be queried/updated wrt features and (un)qualified attributes.
 * Attributes are either booleans or integers.
 */
class FeatureSelection {
  private var _feats = Map[FID,Boolean]() withDefaultValue false
  private var _attrs = Map[AID,Either[Boolean,Int]]()

  def apply(f:String): Boolean             = _feats(FID(f))
  def apply(f:String,a:String): Either[Boolean,Int] =
    _attrs(AIDQ(f,a))
  def apply(f:FID): Boolean             = _feats(f)
  def apply(a:AID): Either[Boolean,Int] = _attrs(a)

  def update(f:String,b:Boolean)    = _feats += (FID(f) -> b)
  def update(f:String,a:String,n:Int) =
    _attrs += (AIDQ(f,a) -> Right(n))
  def update(f:String,a:String,b:Boolean) =
    _attrs += (AIDQ(f,a) -> Left(b))
  def update(f:FID,b:Boolean)       = _feats += (f -> b)
  def update(a:AID,n:Int)           = _attrs += (a -> Right(n))
  def update(a:AID,n:Boolean)       = _attrs += (a -> Left(n))

  //  def update(f:FID,a:AID,n:Int)     = _attrs += (a.addFID(f) -> Right(n))
//  def update(f:FID,a:AID,b:Boolean) = _attrs += (a.addFID(f) -> Left(b))
  
  override def toString = {
      _feats.mkString("[",",","") ++
      (if (_feats.isEmpty) "" else ",") ++
      _attrs.mkString("",",","]")
  }

}
