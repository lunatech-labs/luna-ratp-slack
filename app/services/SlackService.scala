package services

import java.time.format.TextStyle
import java.time.{DayOfWeek, LocalDate}
import java.util.Locale

import com.lunatech.slack.client.api.SlackClient
import com.lunatech.slack.client.models._
import javax.inject.{Inject, Singleton}
import models._
import play.api.{Configuration, Logger}
import repositories.{AlertFormRepository, AlertRepository}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SlackService @Inject()(ratp: RATPService, alertFormRepo: AlertFormRepository, alertRepo: AlertRepository, config: Configuration)(implicit ec: ExecutionContext) {
  val slackClient = SlackClient(config.get[String]("slack.api.token"))


  private def intercalate[T](a: List[T], b: List[T]): List[T] = a match {
    case head :: tail => head :: intercalate(b, tail)
    case _ => b
  }

  private def mapTransport(transport: String) = {
    val transformation = Map(
      "metros" -> "Métro",
      "rers" -> "RER"
    )

    transformation.getOrElse(transport, transport)
  }

  private def getStatusColor(slug: String) = {
    if (slug == "normal") {
      "#4BB543"
    } else if (slug.contains("normal")) {
      "#FFA500"
    } else {
      "#B33A3A"
    }
  }

  def getAlertMessage(alertForm: AlertForm, errorMessage: String = ""): Future[Message] = {
    val days = Map(
      1 -> "Lundi",
      2 -> "Mardi",
      3 -> "Mercredi",
      4 -> "Jeudi",
      5 -> "Vendredi",
      6 -> "Samedi",
      7 -> "Dimanche"
    )

    val now = LocalDate.now.getDayOfWeek

    val fields =
      BasicField("Aujourd'hui", "0") +:
        BasicField("Demain", "1") +:
        (2 to 7).map(now.plus(_)).map(x => BasicField(days(x.getValue), x.getValue.toString))

    val timeTypeMenuBase =
      StaticMenu(s"alertDay_${alertForm.id}", "Quand ?", options = Some(fields))

    val timeTypeMenu = getMenuWithSelectedValue(timeTypeMenuBase, alertForm.alertDay.toString)

    val stationFuture = alertTransportMenu(alertForm)

    stationFuture.flatMap { station =>
      val message = Message("Ajouter une alerte : ")
        .addAttachment(station)
        .addAttachment(alertTypeMenu(alertForm))

      val messageWithButtonsFuture = alertDayButtons(alertForm).map { buttons =>
        if (alertForm.alertType == AlertType.REPEAT) {
          message.addAttachment(buttons.withTitle("Quand ?"))

        } else {
          message
            .addAttachment(AttachmentField("Type of alert", "alert_time")
              .addAction(timeTypeMenu)
              .withTitle("Quand ?"))
        }
      }

      messageWithButtonsFuture.map(messageWithButton =>
        messageWithButton
          .addAttachment(alertTimeMenu(alertForm))
          .addAttachment(AttachmentField("error", "error").withText(errorMessage).withColor("danger"))
          .addAttachment(AttachmentField(s"validation_${alertForm.id}", "validation")
            .addAction(Button(s"validate", "Valider").withValue(alertForm.id).asPrimaryButton)
            .addAction(Button(s"cancel", "Annuler").withValue(alertForm.id).asDangerButton.withConfirmation("Êtes-vous sûr ?")))
      )
    }
  }

  private def alertTypeMenu(alertForm: AlertForm): AttachmentField = {
    val menu = StaticMenu(s"alertType_${alertForm.id}", "Alert type menu")
      .addOption(AlertType.PONCTUAL.toString, AlertType.PONCTUAL.toString)
      .addOption(AlertType.REPEAT.toString, AlertType.REPEAT.toString)

    val menuWithValue = getMenuWithSelectedValue(menu, alertForm.alertType.toString)

    AttachmentField("type", "alert_type")
      .withTitle("Type de l'alerte")
      .addAction(menuWithValue)
  }

  private def alertTimeMenu(alertForm: AlertForm): AttachmentField = {
    AttachmentField("time", "choose_time")
      .addAction(hourMenu(alertForm))
      .addAction(minutesMenu(alertForm))
  }

  private def alertDayButtons(alertForm: AlertForm) = {
    val days = Seq(
      1 -> "Lundi",
      2 -> "Mardi",
      3 -> "Mercredi",
      4 -> "Jeudi",
      5 -> "Vendredi",
      6 -> "Samedi",
      7 -> "Dimanche"
    )

    val daysForAlert = alertFormRepo.getDaysForAlertForm(alertForm.id)

    val futureFields = daysForAlert.map { list =>
      Logger.debug(list.toString())
      val enabledDays = list.map(x => x.getValue)
      days.map(x =>
        if (enabledDays.contains(x._1)) {
          Button(alertForm.id, x._2).withValue(x._1.toString).asPrimaryButton
        } else {
          Button(alertForm.id, x._2).withValue(x._1.toString)
        }
      )
    }

    futureFields.map(fields => AttachmentField("Select a day", "day_button", actions = Some(fields)))
  }

  private def alertTransportMenu(alertForm: AlertForm): Future[AttachmentField] = {
    val station = AttachmentField("station", s"alert_station").withTitle("Station")

    val firstMenuBase =
      StaticMenu(s"type_${alertForm.id}", "Le moyen de transport")
        .addOption(text = "RER", value = "rers")
        .addOption(text = "Métro", value = "metros")

    val firstMenu = alertForm.transportType
      .map(value => getMenuWithSelectedValue(firstMenuBase, value))
      .getOrElse(firstMenuBase)

    val secondMenu = getAlertCode(alertForm)
    val thirdMenu = getAlertStation(alertForm)

    secondMenu.zip(thirdMenu) map {
      case (s, t) =>
        station
          .addAction(firstMenu)
          .addAction(s)
          .addAction(t)
    }

  }

  def disableAlertMessage(userId: String): Future[Message] = {
    val alertsFuture = alertRepo.getAlertForUser(userId)

    alertsFuture
      .map(alerts => alerts.map(a => getAttachmnentForAlertWithDays(a)))
      .map {
        case attachments if attachments.nonEmpty => Message(attachments = Some(attachments))
        case _ => Message("Vous n'avez aucune alerte")
      }
  }

  private def getAttachmnentForAlertWithDays(alert: AlertWithDays) = {
    AttachmentField("Delete alert", "delete_alert")
      .withTitle(s"${ratp.nameOfType(alert.alert.trainType)} ${alert.alert.trainCode} ${alert.alert.station} à ${alert.alert.hour}:${alert.alert.minutes}")
      .withText(alert.days
        .map(days => days.map(x => DayOfWeek.of(x.day).getDisplayName(TextStyle.FULL, Locale.FRANCE).capitalize))
        .map(x => x.mkString(" - ")).getOrElse("")
      )
      .addAction(Button(s"delete", "Supprimer").withValue(alert.alert.id.toString).asDangerButton.withConfirmation("Voulez vous vraiment supprimer cette alerte ?"))
  }


  private def getAlertCode(alertForm: AlertForm) = {
    val menu: Future[StaticMenu] = alertForm.transportType match {
      case Some(transport) => ratp.getCodes(transport) flatMap {
        case TrainResultSuccess(codes) =>
          val fields = codes.map(s => BasicField(s.name, s"${s.code}"))
          Future.successful(StaticMenu(s"code_${alertForm.id}", "Select code", options = Some(fields)))
        case TrainResultError(e) => Future.failed(e)
      }
      case None => Future.successful(StaticMenu(s"code__${alertForm.id}", "Select code"))
    }

    menu.map(m => alertForm.transportCode.map(current => getMenuWithSelectedValue(m, current)).getOrElse(m))
  }

  private def getAlertStation(alertForm: AlertForm) = {
    val menu: Future[StaticMenu] = (alertForm.transportType, alertForm.transportCode) match {
      case (Some(transport), Some(code)) => ratp.getStations(transport, code) flatMap {
        case TrainResultSuccess(stations) =>
          val fields = stations.map(s => BasicField(s.name, s"${s.name}"))
          Future.successful(StaticMenu(s"station_${alertForm.id}", "Select code", options = Some(fields)))
        case TrainResultError(e) => Future.failed(e)
      }
      case _ => Future.successful(StaticMenu(s"station_${alertForm.id}", "Select code"))
    }

    menu.map(m => alertForm.transportStation.map(current => getMenuWithSelectedValue(m, current)).getOrElse(m))
  }

  private def hourMenu(alertForm: AlertForm) = {
    val fields = for {
      i <- 0 until 24
    } yield BasicField(f"$i%02d h", i.toString)

    getMenuWithSelectedValue(StaticMenu(s"hour_${alertForm.id}", "Choisir l'heure", options = Some(fields)), alertForm.hour.getOrElse(0).toString)
  }

  private def minutesMenu(alertForm: AlertForm) = {
    val fields = for {
      i <- 0 until 60
    } yield BasicField(f"$i%02d", i.toString)

    getMenuWithSelectedValue(StaticMenu(s"minute_${alertForm.id}", "Choisir la minute", options = Some(fields)), alertForm.minutes.getOrElse(0).toString)
  }

  def getStatusAttachment(transport: String, status: Status): AttachmentField = {
    AttachmentField("status", "status")
      .withTitle(status.title + " sur la ligne " + mapTransport(transport) + " " + status.line)
      .withText(status.message)
      .withColor(getStatusColor(status.slug))
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

  def selectSubscriptionMessage: Message = {
    subscriptionMessage.addAttachment(subscriptionAttachment.addAction(selectTransportMenu))
  }

  def selectCodeMessage(selectedOption: SelectedOption): Future[Message] = {
    val firstMenu = getMenuWithSelectedValue(selectTransportMenu, selectedOption.value)

    getCodeMenu(selectedOption.value) map {
      menu =>
        selectMessage
          .addAttachment(selectTransportAttachment.addAction(firstMenu).withColor("#4BB543"))
          .addAttachment(selectCodeAttachment.addAction(menu))
    }
  }

  def selectCodeSubscription(selectedOption: SelectedOption): Future[Message] = {
    val firstMenu = getMenuWithSelectedValue(selectTransportMenu, selectedOption.value)

    getCodeMenu(selectedOption.value) map {
      menu =>
        subscriptionMessage
          .addAttachment(selectTransportAttachment.addAction(firstMenu).withColor("#4BB543"))
          .addAttachment(selectCodeSubscriptionAttachment.addAction(menu.withConfirmation("Voulez-vous vraiment vous abonnez à cette ligne ?")))
    }
  }

  def selectStationMessage(selectedOption: SelectedOption): Future[Message] = {
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

  def getSelectTranspotMenu: StaticMenu =
    StaticMenu(name = "Moyen de transport", "Moyen de transport")
      .addOption(text = "RER", value = "rers")
      .addOption(text = "Métro", value = "metros")

  def getStationsMenu(transport: String, code: String): Future[StaticMenu] = {
    ratp.getStations(transport, code) flatMap {
      case TrainResultSuccess(stations) =>
        val fields = stations.map(s => BasicField(s.name, s"${transport}_${code}_${s.name}"))
        Future.successful(selectStationMenu.copy(options = Some(fields)))
      case TrainResultError(e) => Future.failed(e)
    }
  }

  def getCodeMenu(transport: String): Future[StaticMenu] = {
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

    menu.copy(selected_options = Some(Seq(selectedField)))
  }

  private val selectMessage: Message = Message(text = "Selectionnez votre moyen de transport")

  private val subscriptionMessage: Message = Message(text = "Selectionnez la ligne pour laquelle pour souhaitez vous abonnez")

  private val selectTransportAttachment: AttachmentField = AttachmentField(fallback = "Transport Selection", callback_id = "select_transport")

  private val subscriptionAttachment: AttachmentField = AttachmentField(fallback = "Subscription Selection", callback_id = "select_subscription")

  private val selectTransportMenu: StaticMenu =
    StaticMenu(name = "Moyen de transport", "Moyen de transport")
      .addOption(text = "RER", value = "rers")
      .addOption(text = "Métro", value = "metros")
      .addOption(text = "Bus", value = "bus")
      .addOption(text = "Tramway", value = "tramways")

  private val selectCodeAttachment: AttachmentField = AttachmentField(fallback = "Code selection", callback_id = "select_code")
  private val selectCodeSubscriptionAttachment: AttachmentField = AttachmentField(fallback = "Code selection", callback_id = "select_code_subscription")

  private val selectCodeMenu: StaticMenu =
    StaticMenu(name = "Code du transport", "Code du transport")

  private val selectStationAttachment: AttachmentField = AttachmentField(fallback = "station selection", callback_id = "select_station")
  private val selectStationMenu: StaticMenu = StaticMenu(name = "La station", "La station")
}
