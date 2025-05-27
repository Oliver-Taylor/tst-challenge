package com.tst.challenge.model

// It's good practice to add imports if you anticipate needing them,
// but for this specific request, `cats.Show` isn't strictly necessary
// for just defining the case class.
// import cats.Show

/**
  * Represents a query for cabin information.
  *
  * @param cabinCode The code of the cabin to query.
  */
case class CabinQuery(cabinCode: String)

// Optional: If you wanted to provide a Show instance for CabinQuery
// object CabinQuery {
//   implicit val showCabinQuery: Show[CabinQuery] = Show.show(cq => s"CabinQuery(cabinCode=${cq.cabinCode})")
// }
