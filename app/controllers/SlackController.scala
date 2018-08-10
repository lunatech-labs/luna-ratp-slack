package controllers


import java.time.{DayOfWeek, LocalDateTime}

import com.lunatech.slack.client.Parser
import com.lunatech.slack.client.models._
import javax.inject.Inject
import models.{Payload => _, _}
import play.api.libs.json.{JsResult, JsSuccess, Json}
import play.api.mvc._
import play.api.{Configuration, Logger}
import repositories._
import services._
import utils.IdGenerator

import scala.concurrent._
import scala.util.{Failure, Success}

class SlackController @Inject()(
  cc: ControllerComponents,
  config: Configuration,
  ratp: RATPService,
  slackService: SlackService,
  alertService: AlertService,
  subscriptionRepo: TrafficSubscriptionRepository,
  trafficRepo: TrafficRepository,
  alertFormRepository: AlertFormRepository,
  alertRepository: AlertRepository,
  dialogFlowService: DialogFlowService,
  userHomeRepository: UserHomeRepository
)
  (implicit ec: ExecutionContext) extends AbstractController(cc) {


  def events = Action { request =>
    val jsonOpt = request.body.asJson

    val slackEventOpt: Option[JsResult[SlackEvent]] = jsonOpt.map(json => Json.fromJson[SlackEvent](json))

    slackEventOpt match {
      case Some(JsSuccess(s, _)) =>
        if (s.`type` == EventType.MESSAGE) {
          dialogFlowService.forwardBody(jsonOpt)
        } else {
          val userInfo = slackService.slackClient.userInfo(s.user)

          userInfo.map { user =>
            Logger.info(user.toString)
            val name = user.profile.flatMap(x => x.real_name).getOrElse("")

            val sendMessage = (text: String) => slackService.slackClient.postEphemeral(ChatEphemeral(channel = user.id, user = user.id, text = text))

            sendMessage(s"Bonjour <@${user.id}>, bienvenue chez *Lunatech* ! Je suis *RATP BOT*")
              .flatMap(_ => sendMessage("Laisse moi te poser quelques questions afin de t'aider dans tes trajets de tous les jours"))
              .flatMap { _ =>
                val message =
                  ChatEphemeral(channel = user.id, user = user.id, text = "Empruntes-tu le RER ou le Métro pour venir ?")
                    .addAttachment(
                      AttachmentField("Welcome message", "more_information")
                        .addAction(Button("yes", "Oui").asPrimaryButton)
                        .addAction(Button("no", "Non").asDangerButton)
                    )
                slackService.slackClient.postEphemeral(message)
              }
          }
        }
      case _ => Logger.debug(s"[Event: Unhandled event] - $jsonOpt ")
    }

    Ok
  }

  def disableAlert = Action.async { request =>
    val payload = Parser.slashCommand(request.body.asFormUrlEncoded.getOrElse(Map()))

    payload match {
      case Success(p) => slackService.disableAlertMessage(p.user_id).map(message =>
        Ok(Json.toJson(message)))
      case _ => Future.successful(BadRequest)
    }
  }

  def alert = Action.async { request =>
    val payload = Parser.slashCommand(request.body.asFormUrlEncoded.getOrElse(Map()))

    val message: Future[Message] = payload match {
      case Success(s) =>
        val userId = s.user_id
        val alertForm = AlertForm.getFormForTime(IdGenerator.getRandomId(userId), userId, LocalDateTime.now)
        Logger.info(alertForm.id)
        alertFormRepository.create(alertForm)

        slackService.getAlertMessage(alertForm)
      case Failure(e) =>
        Logger.error(e.getMessage)
        Future.successful(slackService.errorMessage(e.getMessage))
    }

    message.map(m => Ok(Json.toJson(m)))
  }

  def unsubscribe = Action.async { request =>
    Logger.info(request.body.toString)
    val payload = Parser.slashCommand(request.body.asFormUrlEncoded.getOrElse(Map()))

    val lines = payload match {
      case Success(s) =>
        val userId = s.user_id
        Logger.info("Unsubscribe caller: " + userId)
        subscriptionRepo.getSubscriptionByUser(userId)

      case Failure(e) =>
        Logger.error(e.getMessage)
        Future.successful(List())
    }

    val message = lines map { subscriptions =>
      Logger.debug(subscriptions.toList.toString)
      val fields = subscriptions.map(subscription => BasicField(s"" +
        s"${ratp.nameOfType(subscription.transport)} ${subscription.line}",
        s"${subscription.transport}_${subscription.line}"))

      if (fields.isEmpty) {
        Message("Vous n'êtes abonné à aucune ligne de la RATP")
      } else {
        val menu = StaticMenu("unsubscribe", "Choisir une ligne", options = Some(fields))
          .withConfirmation("Voulez-vous vraiment vous désabonner ?")

        Message("Choisissez la ligne pour vous désabonnez des alertes.")
          .addAttachment(AttachmentField("select_unsub", "select_unsub").addAction(menu))
      }
    }

    message map (m => Ok(Json.toJson(m)))

  }

  def subscribe = Action {
    Ok(Json.toJson(slackService.selectSubscriptionMessage))
  }

  def nextRER = Action {
    Ok(Json.toJson(slackService.selectTransportMessage))
  }

  def suggestions = Action { request =>
    Logger.info(request.body.toString)
    Ok("")
  }

  def interactive = Action.async { request =>
    val payload = Parser.getPayload(request.body.asFormUrlEncoded.getOrElse(Map()))

    Logger.info(request.body.toString)

    payload match {
      case Success(s) => s.callback_id match {
        // Select next hours at stations
        case "select_transport" => selectCode(s)
        case "select_code" => selectStation(s)
        case "select_station" => showTrains(s)

        // Subscription
        case "select_subscription" => selectSubscription(s)
        case "select_code_subscription" => subscribeToTransport(s)

        // Unsubscription
        case "select_unsub" => unsubscribeToTransport(s)

        // AlertForm
        case "alert_station" => alertFormChangeType(s)
        case "alert_time" => alertFormChangeTime(s)
        case "alert_type" => alertFormChangeAlertType(s)
        case "day_button" => pushButton(s)
        case "choose_time" => chooseTime(s)
        case "validation" => validateAlertForm(s)
        case "delete_alert" => deleteAlert(s)

        // Welcome messages
        case "more_information" => moreInformation(s)
        case "welcome_transport" => welcomeTransport(s)
        case "welcome_code" => welcomeCode(s)
        case "welcome_station" => welcomeStation(s)

        // Not implemented
        //case _ => Future.successful(Ok("Je ne sais pas résoudre cette action"))
        case _ => Future.successful(Ok)
      }
      case Failure(e) => Future.successful(Ok(Json.toJson(slackService.errorMessage(e.getMessage))))
    }
  }

  // TODO Save user's home
  private def welcomeStation(payload: Payload): Future[Result] = {
    val message = (for {
      action <- payload.actions.flatMap(_.headOption)
      options <- action.selected_options.flatMap(_.headOption)
      value <- Some(options.value)
    } yield {
      value.split("_") match {
        case Array(transport, code, station) =>
          Some(userHomeRepository.insertOrUpdate(UserHomeStation(0, payload.user.id, transport, code, station)).map {_ =>
            Message("Génial ! Je peux t'avertir des horaires des trains avant que tu ne partes de chez toi. Pour cela j'ai besoin de ta station de départ.")
              .addAttachment(AttachmentField("", "").withText(s"*${ratp.nameOfType(transport)} $code $station*"))
          })
        case _ => None
      }
    }).flatten

    message.map(_.map(x => Ok(Json.toJson(x)))).getOrElse(Future.successful(Ok("")))
  }

  private def welcomeCode(payload: Payload): Future[Result] = {
    val message = (for {
      action <- payload.actions.flatMap(_.headOption)
      options <- action.selected_options.flatMap(_.headOption)
      value <- Some(options.value)
    } yield {
      Logger.info(value)

      value.split("_") match {
        case Array(transport, code) =>
          val transportMenu = slackService.getSelectTranspotMenu.copy(selected_options = Some(Seq(BasicField(ratp.nameOfType(transport), transport))))
          val codeMenu = slackService.getCodeMenu(transport).map(menu => menu.copy(selected_options = Some(Seq(BasicField(s"${ratp.nameOfType(transport)} $code", code)))))
          val stationMenu = slackService.getStationsMenu(transport, code)

          Some(codeMenu.zip(stationMenu) map { case (c, s) =>
            Message("Génial ! Je peux t'avertir des horaires des trains avant que tu ne partes de chez toi. Pour cela j'ai besoin de ta station de départ.")
              .addAttachment(
                AttachmentField("Moyen de transport", "welcome_station")
                  .addAction(transportMenu)
                  .addAction(c)
                  .addAction(s)
              )
          })

        case _ => None
      }
    }).flatten

    Logger.info(message.toString)

    message.map(_.map(x => Ok(Json.toJson(x)))).getOrElse(Future.successful(Ok("")))
  }

  private def welcomeTransport(payload: Payload) = {

    val message = for {
      action <- payload.actions.flatMap(_.headOption)
      options <- action.selected_options.flatMap(_.headOption)
      value <- Some(options.value)
    } yield {
      Logger.info(value)
      slackService.getCodeMenu(value) map { menu =>
        Message("Génial ! Je peux t'avertir des horaires des trains avant que tu ne partes de chez toi. Pour cela j'ai besoin de ta station de départ.")
          .addAttachment(
            AttachmentField("Moyen de transport", "welcome_code")
              .addAction(slackService.getSelectTranspotMenu.copy(selected_options = Some(Seq(BasicField(ratp.nameOfType(value), value)))))
              .addAction(menu)
          )
      }
    }

    Logger.info(message.toString)

    message.map(_.map(x => Ok(Json.toJson(x)))).getOrElse(Future.successful(Ok("")))
  }

  private def moreInformation(payload: Payload) = {
    val chatEphemeral = (x: String) => ChatEphemeral(channel = payload.user.id, user = payload.user.id, text = x)
    val sendMessage = (x: String) => slackService.slackClient.postEphemeral(chatEphemeral(x))
    val value = for {
      action <- payload.actions.flatMap(x => x.headOption)
    } yield action.name

    val message =
      if (value.contains("yes")) {
        slackService.slackClient.postEphemeral {
          chatEphemeral("Génial ! Je peux t'avertir des horaires des trains avant que tu ne partes de chez toi. Pour cela j'ai besoin de ta station de départ.")
            .addAttachment {
              AttachmentField("welcome station", "welcome_transport")
                .addAction(slackService.getSelectTranspotMenu)
            }
        }.map(_ => Message("Empruntes-tu le RER ou le Métro pour venir ?").addAttachment(AttachmentField("", "").withText("*Oui*")))
      } else if (value.contains("no")) {
        sendMessage("D'accord, je ne vais pas être très utile dans ce cas, n'hésite tout de même pas à revenir me voir !")
          .map(_ => Message("Empruntes-tu le RER ou le Métro pour venir ?").addAttachment(AttachmentField("", "").withText("*Non*")))

      } else {
        Future.successful(Message("Une erreur s'est produite, revenez plus tard."))
      }

    message.map(x => Ok(Json.toJson(x)))
  }

  private def deleteAlert(payload: Payload) = {
    val alertId = for {
      action <- payload.actions.flatMap(_.headOption)
      value <- action.value
    } yield value

    alertId match {
      case Some(id) => alertRepository.delete(id.toInt).map(_ => Ok("Alerte supprimée"))
      case None => Future.successful(Ok(Json.toJson(slackService.errorMessage("Cette aletre n'existe pas"))))
    }
  }

  private def chooseTime(payload: Payload) = {
    Logger.debug("choose time")
    val values = for {
      action <- payload.actions.flatMap(x => x.headOption)
      selectedOptions <- action.selected_options
      value <- selectedOptions.headOption.map(x => x.value)
    } yield (action.name, value)

    val idOpt: Future[Option[String]] = values match {
      case Some((name, value)) if name.trim.startsWith("minute_") =>
        Logger.info("MINUTE")
        alertFormRepository.updateMinutes(name.trim.split("_")(1), value.toInt).map(_ => Some(name.split("_")(1)))
      case Some((name, value)) if name.trim.startsWith("hour_") =>
        Logger.info("Hour")
        alertFormRepository.updateHour(name.trim.split("_")(1), value.toInt).map(_ => Some(name.split("_")(1)))
      case _ => Future.successful(None)
    }

    idOpt flatMap {
      case Some(id) =>
        alertFormRepository.getAlertForm(id)
          .flatMap {
            form =>
              Logger.info(form.toString)
              slackService.getAlertMessage(form) map (message => Ok(Json.toJson(message)))
          }
      case None => Future.successful(Ok)
    }
  } recoverWith {
    case e: Exception => Future.successful(Ok(Json.toJson(slackService.errorMessage(e.getMessage))))
  }

  private def pushButton(payload: Payload) = {
    val values = for {
      action <- payload.actions.flatMap(x => x.headOption)
      value <- action.value
    } yield (action.name, value)

    Logger.debug(values.toString)

    values match {
      case Some((id, value)) =>
        alertFormRepository.createOrDeleteDay(id, DayOfWeek.of(value.toInt)).flatMap {
          _ =>
            alertFormRepository.getAlertForm(id)
              .flatMap {
                form =>
                  Logger.info(form.toString)
                  slackService.getAlertMessage(form) map (message => Ok(Json.toJson(message)))
              }
        }
      case _ => Future.successful(Ok(Json.toJson(Message("Une erreur s'est produite"))))
    }
  }

  private def validateAlertForm(payload: Payload): Future[Result] = {
    val value = for {
      action <- payload.actions.flatMap(x => x.headOption)
      value <- action.value
    } yield (action.name, value)

    value match {
      case Some(("validate", id)) =>
        val alertForm = alertFormRepository.getAlertForm(id)
        val days = alertFormRepository.getDaysForAlertForm(id)

        days.zip(alertForm)
          // Create the alert
          .flatMap {
          case (d, a) => createAlert(a, d)
        }
          // Delete the form
          .flatMap(_ => alertFormRepository.delete(id))
          .map {
            _ => Ok("Créer")
          } recoverWith {
          case e: Exception => alertForm.map(a =>
            slackService
              .getAlertMessage(a, e.getMessage))
            .flatMap(messageFuture => messageFuture.map(message => Ok(Json.toJson(message))))
        }
      case Some(("cancel", id)) =>
        alertFormRepository.delete(id)
          .map(_ => Ok(Json.toJson(Message().andDeleteOriginal)))
      case _ => Future.successful(Ok(Json.toJson(slackService.errorMessage("Action inconnu"))))
    }

  }

  private case class AlertAndDate(alert: Alert, date: LocalDateTime)

  private def getAlertHourMinute(alertForm: AlertForm) =
    for {
      hour <- alertForm.hour
      minute <- alertForm.minutes
      transportType <- alertForm.transportType
      transportCode <- alertForm.transportCode
      transportStation <- alertForm.transportStation
    } yield
      Alert(0,
        alertForm.userId,
        transportType,
        transportCode,
        transportStation,
        hour,
        minute
      )

  private def createAlert(alertForm: AlertForm, days: Seq[DayOfWeek]) = {

    val now = LocalDateTime.now.withSecond(0)

    val alert = getAlertHourMinute(alertForm)

    Logger.info(alert.toString)

    val alertOption: Option[AlertAndDate] =
      if (alertForm.alertType == AlertType.PONCTUAL) {
        alert.map(a => AlertAndDate(a, now.withMinute(a.minutes).withHour(a.hour).plusDays(alertForm.alertDay)))
      } else {
        // RECURRENT ALERT
        val daysUntilFirstSend =
          days
            .map(_.getValue)
            .filter(_ != now.getDayOfWeek.getValue)
            .map {
              value =>
                if (value > now.getDayOfWeek.getValue) {
                  value - now.getDayOfWeek.getValue
                } else {
                  value + 7 - now.getDayOfWeek.getValue
                }
            }.min

        alert.map(a => AlertAndDate(a, now.withMinute(a.minutes).withHour(a.hour).plusDays(daysUntilFirstSend)))
      }

    alertOption match {
      case Some(AlertAndDate(a, d)) =>
        alertRepository.create(a, days: _*)
          .map(id => alertService.scheduleAlert(id, d.withSecond(0)))


      case None => Future.failed(new Exception("Formulaire invalide"))
    }
  }

  private def unsubscribeToTransport(payload: Payload) = {
    val options = payload.actions.flatMap(x => x.headOption.flatMap(x => x.selected_options))

    (options match {
      case Some(opt) if opt.length == 1 =>
        val user = payload.user.id

        opt.head.value.split("_") match {
          case Array(transport, line) =>
            subscriptionRepo.delete(TrafficSubscription(user, transport, line)).map(_ => Ok(s"Vous vous êtes désabonné de la ligne $line"))
          case _ => Future.failed(new Exception("Il faut appeler la commande /unsubscribe"))
        }
    }) recoverWith {
      case _: Exception => Future.successful(Ok(Json.toJson(slackService.errorMessage("Vous êtes déjà abonnés à cette ligne"))))
    }
  }

  private def subscribeToTransport(payload: Payload) = {
    val options = payload.actions.flatMap(x => x.headOption.flatMap(x => x.selected_options))

    (options match {
      case Some(opt) if opt.length == 1 =>
        val user = payload.user.id

        opt.head.value.split("_") match {
          case Array(transport, line) =>
            subscriptionRepo.create(TrafficSubscription(user, transport, line)).map(_ => Ok(s"Vous êtes abonné à la ligne $line"))
          case _ => Future.failed(new Exception("Il faut appeler la commande /subscribe"))
        }

    }) recoverWith {
      case _: Exception => Future.successful(Ok(Json.toJson(slackService.errorMessage("Vous êtes déjà abonnés à cette ligne"))))
    }
  }

  private def showTrains(payload: Payload) = {
    val options = payload.actions.flatMap(x => x.headOption.flatMap(x => x.selected_options))

    (options match {
      case Some(opt) if opt.length == 1 =>
        val values = opt.head

        values.value.split("_") match {
          case Array(transport, code, station) => nextRer(transport, code, station)
          case _ => Future.failed(new Exception("Il faut appeler la commande /RATP"))
        }
    }) recoverWith {
      case e: Exception => Future.successful(Ok(Json.toJson(slackService.errorMessage(e.getMessage))))
    }
  }

  private def getInteractionValue(payload: Payload) = {
    for {
      action <- payload.actions
      firstAction <- action.headOption
      name <- Some(firstAction.name)
      value <- firstAction.selected_options.flatMap(x => x.headOption).map(x => x.value)
    } yield (name, value)
  }

  private def alertFormChangeAlertType(payload: Payload) = {
    val res = getInteractionValue(payload)

    Logger.info("CHANGE TYPE")

    val idOpt = res match {
      case Some((name, value)) if name.startsWith("alertType_") =>
        alertFormRepository.updateAlertType(name.trim.split("_")(1), AlertType.withName(value)).map(_ => Some(name.trim.split("_")(1)))
      case _ =>
        Future.successful(None)
    }

    returnAlertMessage(idOpt)
  }

  private def returnAlertMessage(idOpt: Future[Option[String]]) = {
    idOpt flatMap {
      case Some(id) =>
        alertFormRepository.getAlertForm(id)
          .flatMap {
            form =>
              Logger.info(form.toString)
              slackService.getAlertMessage(form) map (message => Ok(Json.toJson(message)))
          }
      case None => Future.successful(Ok)
    } recoverWith {
      case e: Exception => Future.successful(Ok(Json.toJson(slackService.errorMessage(e.getMessage))))
    }
  }

  private def alertFormChangeTime(payload: Payload) = {
    val res = getInteractionValue(payload)

    val idOpt: Future[Option[String]] = res match {
      case Some((name, value)) if name.startsWith("alertDay_") =>
        alertFormRepository.updateAlertDay(name.trim.split("_")(1), value.toInt).map(_ => Some(name.trim.split("_")(1)))
      case _ =>
        Future.successful(None)
    }

    returnAlertMessage(idOpt)
  }

  private def alertFormChangeType(payload: Payload) = {
    val res = getInteractionValue(payload)

    Logger.info(res.toString)

    val idOpt: Future[Option[String]] = res match {
      case Some((name, value)) if name.trim.startsWith("type_") =>
        Logger.info("TYPE")
        alertFormRepository.updateType(name.trim.split("_")(1), value).map(_ => Some(name.split("_")(1)))
      case Some((name, value)) if name.startsWith("code_") =>
        Logger.info("CODE")
        alertFormRepository.updateCode(name.trim.split("_")(1), value).map(_ => Some(name.split("_")(1)))
      case Some((name, value)) if name.startsWith("station_") =>
        Logger.info("STATION")
        alertFormRepository.updateStation(name.split("_")(1), value).map(_ => Some(name.split("_")(1)))
      case _ =>
        Logger.info("NONE")
        Future.successful(None)
    }

    returnAlertMessage(idOpt)
  }

  private def selectCode(payload: Payload) = {
    select(payload, slackService.selectCodeMessage)
  }

  private def selectStation(payload: Payload) = {
    select(payload, slackService.selectStationMessage)
  }

  private def selectSubscription(payload: Payload) = {
    select(payload, slackService.selectCodeSubscription)
  }

  private def select(payload: Payload, function: SelectedOption => Future[Message])(implicit ec: ExecutionContext): Future[Result] = {
    val options = payload.actions.flatMap(x => x.headOption.flatMap(x => x.selected_options))

    val message = options match {
      case Some(opt) if opt.length == 1 =>
        val transport = opt.head

        function(transport).map(message => Ok(Json.toJson(message)))
      case _ => Future.successful(Ok(Json.toJson(slackService.errorMessage("No train found"))))
    }

    message recoverWith {
      case e: Exception => Future.successful(Ok(Json.toJson(slackService.errorMessage(e.getMessage))))
    }
  }

  private def nextRer(transport: String, code: String, station: String): Future[Result] = {
    val destination = TrainDestination(transport, code, station)

    ratp.nextTrain(destination) map {
      case TrainResultSuccess(s) => {
        val attachments = slackService.toAttachmentNextTrains(s, "trains", "trains")

        val message = Message(Some(s"_Voici les prochains *${
          transport.toUpperCase
        } $code* à *$station*_")).addAttachment(attachments)

        Ok(Json.toJson(message))
      }

      case TrainResultError(e) => Ok(
        Json.toJson(slackService.errorMessage(e.getMessage))
      )
    }
  }

}
