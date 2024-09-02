val toolkitV    = "0.1.3"
val toolkit     = "org.typelevel" %% "toolkit"      % toolkitV
val toolkitTest = "org.typelevel" %% "toolkit-test" % toolkitV

ThisBuild / scalaVersion := "2.13.14"
libraryDependencies += toolkit
libraryDependencies += (toolkitTest % Test)

addCommandAlias("problem1", "runMain com.tst.challenge.domain.CruisePriceService")
addCommandAlias("problem2", "runMain com.tst.challenge.domain.CruisePromotionService")
