package services

import com.lunatech.slack.client.models._
import javax.inject.{Inject, Singleton}
import models.TrainSchedule
import play.api.Logger

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SlackService @Inject()(ratp: RATPService) {

  private def intercalate[T](a: List[T], b: List[T]): List[T] = a match {
    case head :: tail => head :: intercalate(b, tail)
    case _ => b
  }

  def toAttachmentNextTrains(trains: Seq[TrainSchedule], fallback: String, callback: String): AttachmentField = {
    val middle = trains.length / 2

    val fields = intercalate(trains.slice(0, middle).toList, trains.slice(middle, trains.length).toList)
      .map(train => Field(title = train.destination, value = train.message).asShort)

    AttachmentField(fallback = fallback, callback_id = callback, fields = Some(fields))
  }

  def errorMessage(message: String): Message = {
    Message("Je suis desolé je ne peux pas vous aider pour ça")
      .addAttachment(AttachmentField("error", "error").withText(message).withColor("danger"))
  }

  def selectTransportMessage: Message = {
    selectMessage.addAttachment(selectTransportAttachment.addAction(selectTransportMenu))
  }

  def selectCodeMessage(selectedOption: SelectedOption)(implicit ec: ExecutionContext): Future[Message] = {
    val firstMenu = getMenuWithSelectedValue(selectTransportMenu, selectedOption.value)


    getCodeMenu(selectedOption.value) map {
      menu =>
        selectMessage
          .addAttachment(selectTransportAttachment.addAction(firstMenu).withColor("#4BB543"))
          .addAttachment(selectCodeAttachment.addAction(menu))
    }
  }

  def selectStationMessage(selectedOption: SelectedOption)(implicit ec: ExecutionContext): Future[Message] = {
    val params = selectedOption.value.split("_")

    if (params.length != 2) {
      return Future.failed(new Exception("Je ne comprends pass"))
    }

    val transport = params(0)
    val code = params(1)

    Logger.info(transport)
    Logger.info(code)

    val firstMenu = getMenuWithSelectedValue(selectTransportMenu, transport)
    getCodeMenu(transport) flatMap { codeMenu =>
      val secondMenu = getMenuWithSelectedValue(codeMenu, selectedOption.value)
      getStationsMenu(transport, code) map { thirdMenu =>
        selectMessage
          .addAttachment(selectTransportAttachment.addAction(firstMenu).withColor("#4BB543"))
          .addAttachment(selectCodeAttachment.addAction(secondMenu).withColor("#4BB543"))
          .addAttachment(selectStationAttachment.addAction(thirdMenu))
      }

    }
  }

  private def getStationsMenu(transport: String, code: String)(implicit ec: ExecutionContext): Future[StaticMenu] = {
    ratp.getStations(transport, code) flatMap {
      case TrainResultSuccess(stations) =>
        val fields = stations.map(s => BasicField(s.name, s"${transport}_${code}_${s.name}"))
        Future.successful(selectStationMenu.copy(options = Some(fields)))
      case TrainResultError(e) => Future.failed(e)
    }
  }

  private def getCodeMenu(transport: String)(implicit ec: ExecutionContext): Future[StaticMenu] = {
    ratp.getCodes(transport) flatMap {
      case TrainResultSuccess(codes) => {
        val fields = codes.map(code => BasicField(code.name, s"${transport}_${code.code}"))
        Future.successful(selectCodeMenu.copy(options = Some(fields)))
      }
      case TrainResultError(e) => Future.failed(e)
    }
  }

  private def getMenuWithSelectedValue(menu: StaticMenu, value: String): StaticMenu = {
    val selectedField = menu
      .options
      .getOrElse(Seq()).find(f => f.value.equals(value)) match {
      case Some(field) => field
      case _ => BasicField("", "")
    }

    StaticMenu(selectTransportMenu.name,
      selectTransportMenu.text,
      options = menu.options,
      selected_options = Some(Seq(selectedField)))
  }

  private val selectMessage: Message = Message(text = "Selectionnez votre moyen de transport")

  private val selectTransportAttachment: AttachmentField = AttachmentField(fallback = "Transport Selection", callback_id = "select_transport")

  private val selectTransportMenu: StaticMenu =
    StaticMenu(name = "Moyen de transport", "Moyen de transport")
      .addOption(text = "RER", value = "rers")
      .addOption(text = "Métro", value = "metros")
      .addOption(text = "Bus", value = "bus")
      .addOption(text = "Tramway", value = "tramways")

  private val selectCodeAttachment: AttachmentField = AttachmentField(fallback = "Code selection", callback_id = "select_code")

  private val selectCodeMenu: StaticMenu =
    StaticMenu(name = "Code du transport", "Code du transport")

  private val selectStationAttachment: AttachmentField = AttachmentField(fallback = "station selection", callback_id = "select_station")
  private val selectStationMenu: StaticMenu = StaticMenu(name = "La station", "La station")
}
