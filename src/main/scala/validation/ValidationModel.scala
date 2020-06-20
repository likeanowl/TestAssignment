package validation

import java.util.UUID

import cats.data._
import cats.implicits._
import io.circe._
import io.circe.parser._

sealed trait DealType

case object Rent extends DealType

case object Sell extends DealType

object DealType {
  def apply(str: String): DealType = str.toLowerCase match {
    case "rent" => Rent
    case "sell" => Sell
  }
}

sealed trait RentType

case object Monthly extends RentType

case object Daily extends RentType

object RentType {
  def apply(str: String): RentType = str.toLowerCase match {
    case "monthly" => Monthly
    case "daily" => Daily
  }
}

sealed trait ObjectType

case object Room extends ObjectType

case object Flat extends ObjectType

object ObjectType {
  def apply(str: String): ObjectType = str.toLowerCase match {
    case "room" => Room
    case "flat" => Flat
  }
}

sealed trait ObjectData

case class FlatData(size: Double, livingSize: Option[Double] = None, kitchenSize: Option[Double] = None, roomsCount: Option[Int] = None) extends ObjectData

case class RoomData(size: Double, roomsCount: Option[Int] = None, kitchenSize: Option[Double] = None) extends ObjectData

case class SellerData(name: String, phone: String)

case class Advertisement(uuid: UUID, price: Double, rentType: Option[RentType] = None, dealType: DealType, objectType: ObjectType,
                         objectData: ObjectData, address: String, sellerData: SellerData)

object AdJsonFields {
  val UUID_FIELD = "uuid"
  val PRICE = "price"
  val DEAL_TYPE = "dealType"
  val RENT_TYPE = "rentType"
  val OBJECT_TYPE = "objectType"
  val OBJECT_DATA = "objectData"
  val ROOM_SIZE = "roomSize"
  val ROOMS_COUNT = "roomsCount"
  val KITCHEN_SIZE = "kitchenSize"
  val FLAT_SIZE = "flatSize"
  val FLAT_LIVING_SIZE = "flatLivingSize"
  val ADDRESS = "address"
  val SELLER_DATA = "sellerData"
  val SELLER_NAME = "sellerName"
  val SELLER_PHONE = "sellerPhone"
}

object ValidationModel {

  import AdJsonFields._

  type ValidationResult[A] = ValidatedNec[DomainValidation, A]

  private val dealTypesSet = Set("rent", "sell")
  private val rentTypesSet = Set("daily", "monthly")
  private val objectTypesSet = Set("room", "flat")

  //https://stackoverflow.com/a/56450924/6301200
  private val phoneNumberRegex = "^(\\+\\d{1,2}\\s?)?1?\\-?\\.?\\s?\\(?\\d{3}\\)?[\\s.-]?\\d{3}[\\s.-]?\\d{4}$"

  private val personNameRegex = "^[a-zA-Z]+(([',. -][a-zA-Z ])?[a-zA-Z]*)*$"

  private val addressRegex = "^[#.0-9a-zA-Z\\s,-]+$"
  //https://stackoverflow.com/a/14166194/6301200
  private val uuidRegex = "[\\w]{8}(-[\\w]{4}){3}-[\\w]{12}"

  private val flatObjectType: ValidationResult[ObjectType] = Flat.validNec
  private val rentDealType: ValidationResult[DealType] = Rent.validNec
  private val roomObjectType: ValidationResult[ObjectType] = Room.validNec
  private val sellDealType: ValidationResult[DealType] = Sell.validNec
  private val rentTypeStub: ValidationResult[Option[RentType]] = None.validNec

  private def wrapParsingError(t: Throwable) = ParsingWentWrong(t).invalidNec

  private def validateUUID(uuid: String): ValidationResult[UUID] = {
    if (uuid.matches(uuidRegex)) UUID.fromString(uuid).validNec else UUIDHasSpecialCharacters.invalidNec
  }

  private def validatePrice(price: Double): ValidationResult[Double] = {
    if (price < 0) PriceIsLowerThanZero.invalidNec else price.validNec
  }

  private def validateDealType(dealType: String): ValidationResult[DealType] = {
    if (dealTypesSet.contains(dealType.toLowerCase)) DealType(dealType).validNec else DealTypeIsUnknown.invalidNec
  }

  private def validateRentType(rentType: String): ValidationResult[RentType] = {
    if (rentTypesSet.contains(rentType.toLowerCase)) RentType(rentType).validNec else RentTypeIsUnknown.invalidNec
  }

  private def validateObjectType(objectType: String): ValidationResult[ObjectType] = {
    if (objectTypesSet.contains(objectType.toLowerCase)) ObjectType(objectType).validNec else ObjectTypeIsUnknown.invalidNec
  }

  private def validateRoomSize(roomSize: Double): ValidationResult[Double] = {
    if (roomSize < 0) RoomSizeIsLowerThanZero.invalidNec else roomSize.validNec
  }

  private def validateFlatSize(flatSize: Double): ValidationResult[Double] = {
    if (flatSize < 0) FlatSizeIsLowerThanZero.invalidNec else flatSize.validNec
  }

  private def validateRoomsCount(roomsCount: Int): ValidationResult[Option[Int]] = {
    if (roomsCount == Int.MinValue)
      None.validNec
    else if (roomsCount < 0) RoomsCountIsLowerThanZero.invalidNec else Option(roomsCount).validNec
  }

  private def validateKitchenSize(kitchenSize: Double): ValidationResult[Option[Double]] = {
    if (kitchenSize == Double.NaN)
      None.validNec
    else if (kitchenSize < 0) KitchenSizeIsLowerThanZero.invalidNec else Option(kitchenSize).validNec
  }

  private def validateFlatLivingSize(livingSize: Double): ValidationResult[Option[Double]] = {
    if (livingSize == Double.NaN)
      None.validNec
    else if (livingSize < 0) FlatLivingSizeIsLowerThanZero.invalidNec else Option(livingSize).validNec
  }

  private def validateAddress(address: String): ValidationResult[String] = {
    if (address.matches(addressRegex)) address.validNec else AddressContainsSpecialCharacters.invalidNec
  }

  private def validateSellersName(name: String): ValidationResult[String] = {
    if (name.matches(personNameRegex)) name.validNec else SellerNameContainsSpecialCharacters.invalidNec
  }

  private def validateSellersPhone(phone: String): ValidationResult[String] = {
    if (phone.matches(phoneNumberRegex)) phone.validNec else SellersPhoneNumberIsIncorrect.invalidNec
  }

  def validateAd(rawJsonString: String): ValidationResult[Advertisement] = {
    import cats.implicits._

    parse(rawJsonString) match {
      case Left(value) => ParsingWentWrong(value.underlying).invalidNec
      case Right(value) =>
        val cursor = value.hcursor
        val dealType = cursor.get[String]("dealType").map(validateDealType).fold(wrapParsingError, identity)
        val objectType = cursor.get[String]("objectType").map(validateObjectType).fold(wrapParsingError, identity)
        (dealType, objectType).mapN { (dt, ot) =>
          (dt, ot) match {
            case (Rent, Flat) => composeRentFlatAd(cursor)
            case (Rent, Room) => composeRentRoomAd(cursor)
            case (Sell, Flat) => composeSellFlatAd(cursor)
            case (Sell, Room) => composeSellRoomAd(cursor)
          }
        }.fold(nec => nec.foldLeft(Chained())((f, acc) => Chained(f.errorMessage + "\n" + acc.errorMessage)).invalidNec, identity)
    }
  }

  private def composeFlatData(cursor: HCursor): ValidationResult[FlatData] = (
    cursor.downField(OBJECT_DATA).get[Double](FLAT_SIZE),
    cursor.downField(OBJECT_DATA).getOrElse[Double](FLAT_LIVING_SIZE)(Double.NaN),
    cursor.downField(OBJECT_DATA).getOrElse[Double](KITCHEN_SIZE)(Double.NaN),
    cursor.downField(OBJECT_DATA).getOrElse[Int](ROOMS_COUNT)(Int.MinValue)
    ).mapN((size, livingSize, kitchenSize, roomsCount) => (
    validateFlatSize(size),
    validateFlatLivingSize(livingSize),
    validateKitchenSize(kitchenSize),
    validateRoomsCount(roomsCount)
    ).mapN(FlatData)
  ).fold(wrapParsingError, identity)

  private def composeRoomData(cursor: HCursor): ValidationResult[RoomData] = (
    cursor.downField(OBJECT_DATA).get[Double](ROOM_SIZE),
    cursor.downField(OBJECT_DATA).getOrElse[Int](ROOMS_COUNT)(Int.MinValue),
    cursor.downField(OBJECT_DATA).getOrElse[Double](KITCHEN_SIZE)(Double.NaN)
    ).mapN((roomSize, roomsCount, kitchenSize) => (
    validateRoomSize(roomSize),
    validateRoomsCount(roomsCount),
    validateKitchenSize(kitchenSize)
    ).mapN(RoomData)
  ).fold(wrapParsingError, identity)

  private def composeSellerData(cursor: HCursor): ValidationResult[SellerData] = (
    cursor.downField(SELLER_DATA).get[String](SELLER_NAME),
    cursor.downField(SELLER_DATA).get[String](SELLER_PHONE)
    ).mapN((sellersName, sellersPhone) => (
    validateSellersName(sellersName),
    validateSellersPhone(sellersPhone)
    ).mapN(SellerData)
  ).fold(wrapParsingError, identity)

  private def composeRentFlatAd(cursor: HCursor): ValidationResult[Advertisement] = {
    val flatData = composeFlatData(cursor)

    val sellerData = composeSellerData(cursor)

    val uuid = cursor.get[String](UUID_FIELD).map(validateUUID).fold(wrapParsingError, identity)
    val price = cursor.get[Double](PRICE).map(validatePrice).fold(wrapParsingError, identity)
    val rentType = cursor.get[String](RENT_TYPE).map(validateRentType).fold(wrapParsingError, identity)
    val address = cursor.get[String](ADDRESS).map(validateAddress).fold(wrapParsingError, identity)

    (
      uuid,
      price,
      rentType.map(rt => Option(rt)),
      rentDealType,
      flatObjectType,
      flatData,
      address,
      sellerData
      ).mapN(Advertisement)
  }

  private def composeSellFlatAd(cursor: HCursor): ValidationResult[Advertisement] = {
    val flatData = composeFlatData(cursor)

    val sellerData = composeSellerData(cursor)

    val uuid = cursor.get[String](UUID_FIELD).map(validateUUID).fold(wrapParsingError, identity)
    val price = cursor.get[Double](PRICE).map(validatePrice).fold(wrapParsingError, identity)
    val address = cursor.get[String](ADDRESS).map(validateAddress).fold(wrapParsingError, identity)

    (
      uuid,
      price,
      rentTypeStub,
      sellDealType,
      flatObjectType,
      flatData,
      address,
      sellerData
      ).mapN(Advertisement)
  }

  private def composeRentRoomAd(cursor: HCursor): ValidationResult[Advertisement] = {
    val roomData = composeRoomData(cursor)

    val sellerData = composeSellerData(cursor)

    val uuid = cursor.get[String](UUID_FIELD).map(validateUUID).fold(wrapParsingError, identity)
    val price = cursor.get[Double](PRICE).map(validatePrice).fold(wrapParsingError, identity)
    val rentType = cursor.get[String](RENT_TYPE).map(validateRentType).fold(wrapParsingError, identity)
    val address = cursor.get[String](ADDRESS).map(validateAddress).fold(wrapParsingError, identity)

    (
      uuid,
      price,
      rentType.map(rt => Option(rt)),
      rentDealType,
      roomObjectType,
      roomData,
      address,
      sellerData
      ).mapN(Advertisement)
  }

  private def composeSellRoomAd(cursor: HCursor): ValidationResult[Advertisement] = {
    val roomData = composeRoomData(cursor)

    val sellerData = composeSellerData(cursor)

    val uuid = cursor.get[String](UUID_FIELD).map(validateUUID).fold(wrapParsingError, identity)
    val price = cursor.get[Double](PRICE).map(validatePrice).fold(wrapParsingError, identity)
    val address = cursor.get[String](ADDRESS).map(validateAddress).fold(wrapParsingError, identity)

    (
      uuid,
      price,
      rentTypeStub,
      sellDealType,
      roomObjectType,
      roomData,
      address,
      sellerData
      ).mapN(Advertisement)
  }
}
