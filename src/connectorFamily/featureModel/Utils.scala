package connectorFamily.featureModel

object Utils {
    
  implicit def str2FID(str:String): FID = FID(str)   
  implicit def str2AID(str:String): AID = AIDU(str)   
  implicit def str2ArrowAID(str: String): ArrowAssoc[AID] =
      new ArrowAssoc(str2AID(str))

  implicit def str2AIDTerm(str:String): AIDTerm = AIDTerm(AIDU(str))
  implicit def str2FeatTerm(str:String): FIDExp = FIDExp(FID(str))
  implicit def int2IntVal(n:Int): IntVal = IntVal(n)
  
  implicit def range2attrRange(r:Range): AttrRange =
	if (r.step == 1) IntAttrBounded(r.start,r.end)
	else IntAttrSet(r.toIterable)

}