import cats.Apply
import cats.data.Validated
import cats.implicits._
import cats.data.Validated.{Invalid, Valid}

//Either example exercise
def validateEmail(e: String): Either[String, String] = {
  if (e.contains('@')) Right(e)
  else Left("wrong")
}

def validatePhone(p: String): Either[String, String] = {
  if (p.forall(_.isDigit)) Right(p)
  else Left("BOOHHHHH")
}

validateEmail("rich")
validateEmail("rich@gmail.com")

//def validateEmailAndPhone(p: String, e: String): Either[List[String], (String, String)] = {
//  (validateEmail(e), validatePhone(p)) match {
//    case (R)
//  }
//}

def validatedEmail(e: String): Validated[List[String], String] = {
  if (e.contains('@')) Valid(e)
  else Invalid(List(e))
}

def validatedPhone(p: String): Validated[List[String], String] = {
  if (p.forall(_.isDigit)) Valid(p)
  else Invalid(List(p))
}

case class Data(email: String, phone: String)

def validatedEmailAndPhone(e: String, p: String): Validated[List[String], Data] = {
  //NB all below are good :)
//  (validatedEmail(e) |@| validatedPhone(p)).map((e: String, p: String) => Data(e, p))
  (validatedEmail(e) |@| validatedPhone(p)).map(Data) // will reference Data.apply
}


validatedEmailAndPhone("rich@gmail.come","2385409234a")
