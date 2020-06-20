package validation

sealed trait DomainValidation {
  def errorMessage: String
}

case class Chained(msg: String = "") extends DomainValidation {
  override def errorMessage: String = msg
}

case class ParsingWentWrong(underlying: Throwable) extends DomainValidation {
  override def errorMessage: String = underlying.getMessage
}

case object UUIDHasSpecialCharacters extends DomainValidation {
  def errorMessage: String = "UUID cannot contain special characters."
}

case object PriceIsLowerThanZero extends DomainValidation {
  override def errorMessage: String = "Price cannot be lower than zero"
}

case object DealTypeIsUnknown extends DomainValidation {
  override def errorMessage: String = "Deal type could be only rent or sell"
}

case object RentTypeIsUnknown extends DomainValidation {
  override def errorMessage: String = "Rent type could be only monthly or daily"
}

case object RoomSizeIsLowerThanZero extends DomainValidation {
  override def errorMessage: String = "Room size cannot be lower than zero"
}

case object RoomsCountIsLowerThanZero extends DomainValidation {
  override def errorMessage: String = "Rooms count cannot be lower than zero"
}

case object ObjectTypeIsUnknown extends DomainValidation {
  override def errorMessage: String = "Object can be only flat or room"
}

case object KitchenSizeIsLowerThanZero extends DomainValidation {
  override def errorMessage: String = "Kitchen size cannot be lower than zero"
}

case object FlatSizeIsLowerThanZero extends DomainValidation {
  override def errorMessage: String = "Flat size cannot be lower than zero"
}

case object FlatLivingSizeIsLowerThanZero extends DomainValidation {
  override def errorMessage: String = "Flat living size cannot be lower than zero"
}

case object AddressContainsSpecialCharacters extends DomainValidation {
  override def errorMessage: String = "Address cannot contain special characters"
}

case object SellerNameContainsSpecialCharacters extends DomainValidation {
  override def errorMessage: String = "Seller name cannot contain special characters"
}

case object SellersPhoneNumberIsIncorrect extends DomainValidation {
  override def errorMessage: String = "Seller phone number is incorrect"
}