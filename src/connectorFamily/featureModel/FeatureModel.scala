package connectorFamily.featureModel

/**
 * A feature model has:
 *   a unique ID,
 *   a (possibly empty) group of sub-features,
 *   a (possibly empty) set of declarations of attributes,
 *   a (possibly empty) set of constraints - NOT HERE YET
 */
case class FeatureModel(fid:FID,gr:Group,at:Attrs,ct:AttrConstr*)
//{
////  def this(name:String) = this(FID(name),EmptyGroup,Attrs())
//}

/** feature ID **/
case class FID(val name:String)
/** Attribute ID **/
class AID(fid:Option[String],nm:String) {
	def name = fid match {
		case Some(n) => n +"."+nm
		case _ => nm
	}
}
/** Qualified AID (with the feature name) **/
case class AIDQ(fid:String,aid:String) extends AID(Some(fid),aid)
/** Unqualified AID **/
case class AIDU(aid:String) extends AID(None,aid)


/** Group of feature models with a cardinality **/
sealed class Group(feats:List[FeatureModel])
case object EmptyGroup extends Group(List())
case class OneOf(feats:List[FeatureModel]) extends Group(feats)
case class AllOf(feats:List[FeatureModel]) extends Group(feats)
case class FromTo(from:Int,to:Int,feats:List[FeatureModel]) extends Group(feats)

/** Declaration of attribute variables with a given range of values **/
case class Attrs(decl:(AID,AttrRange)*)

/** Range of values for an attribute - only bounded integers so far. **/
sealed abstract class AttrRange()
case object IntAttr extends AttrRange
case class IntAttrTo(to:Int) extends AttrRange
case class IntAttrFrom(from:Int) extends AttrRange
case class IntAttrBounded(from:Int,to:Int) extends AttrRange
case class IntAttrSet(s:Iterable[Int]) extends AttrRange
case object BoolAttr extends AttrRange


//// Builders for feature models and cardinalities ////
object FeatureModel {
  def apply(name:String): FeatureModel =
  	FeatureModel(FID(name),EmptyGroup,Attrs()) 
  def apply(name:String,gr:Group): FeatureModel =
    FeatureModel(FID(name),gr,Attrs()) 
//  def apply(fid:FID,gr:Group,at:Attrs): FeatureModel =
//    FeatureModel(fid,gr,at,True) 
}
object OneOf   { def apply(feats:FeatureModel*): OneOf = OneOf(feats.toList) }
object AllOf   { def apply(feats:FeatureModel*): AllOf = AllOf(feats.toList) }
object FromTo  { def apply(from:Int,to:Int,feats:FeatureModel*): FromTo = FromTo(from,to,feats.toList) }
