package validation

import java.util.UUID

import org.scalatest.flatspec.AnyFlatSpec

class ValidationModelSpec extends AnyFlatSpec {

  it should "work correctly in basic cases" in {
    val examples = Seq(
      """{
            "uuid": "123e4567-e89b-12d3-a456-426614174000",
            "price": 555555.0,
            "dealType": "Rent",
            "rentType": "Monthly",
            "objectType": "Room",
            "objectData": {
              "roomSize": 39.4,
              "roomsCount": 4,
              "kitchenSize": 14.5
            },
            "address": "Kushelevskaya doroga, dom 32, kv 15",
            "sellerData": {
              "sellerName": "Bobby Bobby",
              "sellerPhone": "+78005553535"
            }
        }
      """,
      """{
            "uuid": "123e4567-e89b-12d3-a456-426614174000",
            "price": 555555.0,
            "dealType": "Sell",
            "objectType": "Room",
            "objectData": {
              "roomSize": 39.4,
              "roomsCount": 4,
              "kitchenSize": 14.5
            },
            "address": "Kushelevskaya doroga, dom 32, kv 15",
            "sellerData": {
              "sellerName": "Bobby Bobby",
              "sellerPhone": "+78005553535"
            }
        }
      """,
      """{
            "uuid": "123e4567-e89b-12d3-a456-426614174000",
            "price": 555555.0,
            "dealType": "Sell",
            "objectType": "Flat",
            "objectData": {
              "flatSize": 39.4,
              "flatLivingSize": 33,
              "roomsCount": 4,
              "kitchenSize": 14.5
            },
            "address": "Kushelevskaya doroga, dom 32, kv 15",
            "sellerData": {
              "sellerName": "Bobby Bobby",
              "sellerPhone": "+78005553535"
            }
        }
      """
    )

    val expected = Seq(
      Advertisement(
        UUID.fromString("123e4567-e89b-12d3-a456-426614174000"),
        555555.0,
        Option(Monthly),
        Rent,
        Room,
        RoomData(
          39.4, Option(4), Option(14.5)
        ),
        "Kushelevskaya doroga, dom 32, kv 15",
        SellerData("Bobby Bobby", "+78005553535")
      ),
      Advertisement(
        UUID.fromString("123e4567-e89b-12d3-a456-426614174000"),
        555555.0,
        None,
        Sell,
        Room,
        RoomData(
          39.4, Option(4), Option(14.5)
        ),
        "Kushelevskaya doroga, dom 32, kv 15",
        SellerData("Bobby Bobby", "+78005553535")
      ),
      Advertisement(
        UUID.fromString("123e4567-e89b-12d3-a456-426614174000"),
        555555.0,
        None,
        Sell,
        Flat,
        FlatData(
          39.4, Option(33.0), Option(14.5), Option(4)
        ),
        "Kushelevskaya doroga, dom 32, kv 15",
        SellerData("Bobby Bobby", "+78005553535")
      ),
    )

    examples.map(ValidationModel.validateAd)
      .map(_.getOrElse(null) /*Meh, but it is a test...*/)
      .zip(expected).foreach(pair => assertResult(pair._2)(pair._1))
  }

  it should "handle and log errors if they occur" in {
    val wrongJson =
      """{
            "uuid": "123e4567-e89b-12d3-a456-426614174000",
            "price": -555555.0,
            "dealType": "SomethingWrong",
            "rentType": "Monthly",
            "objectType": "Room",
            "objectData": {
              "roomSize": 39.4,
              "roomsCount": 4,
              "kitchenSize": "large"
            },
            "address": "Kushelevskaya doroga, dom 32, kv 15",
            "sellerData": {
              "sellerName": "Bobby Bobby",
              "sellerPhone": "+78005553535"
            }
        }
      """

    val result = ValidationModel.validateAd(wrongJson)
      .fold(identity, null).foldLeft("")((v, acc) => acc + v).trim
    val expected = "Chained(\nDeal type could be only rent or sell)"
    assertResult(expected)(result)
  }

}
