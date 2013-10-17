package connectorFamily.featureModel

import org.junit.Test
import org.junit.Assert._

class TestFeatureModel {
  import Utils._
    
  val mergerFM1 =
    FeatureModel (
      "Merger",
      AllOf(
        FeatureModel("Simple"),
        FeatureModel("MaxMerger") ),        
      Attrs( "size" -> (1 until 10))
    )

  val mergerFM2 =
    FeatureModel (
      "Merger",
      AllOf( List(
        FeatureModel("Simple"),
        FeatureModel("MaxMerger")) ),
      Attrs( "size" -> IntAttrBounded(1,10)) // this line is different.
    )
    
    val mergerFM3 =
      FeatureModel (
        "Merger",
        AllOf(
          FeatureModel("Simple"),
          FeatureModel("MaxMerger") ),        
        Attrs( "size"   -> (1 until 10),
        		   "isExcl" -> BoolAttr    ), // these 3 lines are new.
        "Merger.size" % 2 === 0,          // 
        "isExcl" === "Simple"             //
      )

    
  /** simple test that will break once the structure/naming changes */
  @Test def printExample{
    println(mergerFM3)

    assertEquals(mergerFM1.toString,
  			"FeatureModel(FID(Merger),AllOf(List("+
  			    "FeatureModel(FID(Simple),EmptyGroup,Attrs(List()),WrappedArray()), "+
  			    "FeatureModel(FID(MaxMerger),EmptyGroup,Attrs(List()),WrappedArray()))),"+
  			  "Attrs(WrappedArray((AIDU(size),IntAttrBounded(1,10)))),WrappedArray())")  
  }

  @Test def equalExamples{
  	  assertEquals(mergerFM1, mergerFM2)
  }


}