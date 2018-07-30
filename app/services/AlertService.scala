package services

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit._

import akka.actor.{ActorSystem, Cancellable}
import com.lunatech.slack.client.models.ChatEphemeral
import javax.inject.Inject
import models.TrainDestination
import play.api.Logger
import repositories.AlertRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

class AlertService @Inject() (actorSystem: ActorSystem, alertRepository: AlertRepository, ratp: RATPService, slackService: SlackService)(implicit ec: ExecutionContext) {

  def scheduleAlert(alertId: Int, date: LocalDateTime): Cancellable = {
    val now = LocalDateTime.now

    val offset = now.until(date, SECONDS)
    actorSystem.scheduler.scheduleOnce(Duration.create(offset, "s"), () => fireAlert(alertId))
  }

  def fireAlert(id: Int): Unit = {
    alertRepository
      .getAlert(id)
      .map{
        case (alert, days) =>
          Logger.info(alert.toString)
          Logger.info(days.toString)
          //TODO check if it still needs to be send today
          // Send the message
          ratp.nextTrain(TrainDestination(trainType = alert.trainType, trainCode = alert.trainCode, station = alert.station)) map {
            case TrainResultSuccess(s) =>
              val attachmentField = slackService.toAttachmentNextTrains(s, "trains", "trains")

              val message =
                ChatEphemeral(alert.userId, s"_Une alerte pour le *${ratp.nameOfType(alert.trainType)}* *${alert.trainCode}* à *${alert.station}*_", alert.userId)
              slackService.slackClient.postEphemeral(message.addAttachment(attachmentField))
            case TrainResultError(_) =>
          }

          if(days.nonEmpty){
            val currentDate = LocalDateTime.now
            val currentDay = currentDate.getDayOfWeek
            val nextDay = days
              .map(x => {
                if(x.getValue == currentDay.getValue){
                  7
                } else if (x.getValue < currentDay.getValue) {
                  x.getValue + 7 - currentDay.getValue
                } else {
                  x.getValue - currentDay.getValue
                }
              })
              .min

            scheduleAlert(id, currentDate.plusDays(nextDay))
          } else {
            // This is a ponctual alert so it needs to be deleted
            alertRepository.delete(id)
          }
      }
  }



}
